package pill

import java.time._
import java.util.concurrent._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import cats.syntax.either._
import com.typesafe.scalalogging.{LazyLogging, StrictLogging}
import pill.data._, pill.exec._

trait Master {

  def info: MasterInfo
  def send(msg: Master.Message): Unit

  def toggle(active: Boolean): Unit = send(Master.Toggle(active))
  def shutdown(): Unit = send(Master.Shutdown)
  def awaitTermination(wait: FiniteDuration): Unit
}

object Master extends StrictLogging {
  type JobList = () => Either[Exception, Vector[ScheduledJob]]

  sealed trait Message
  case object Shutdown extends Message
  case class Toggle(active: Boolean) extends Message
  case object EvalTrigger extends Message
  case class JobDone(id: String) extends Message
  case class RunNow(jobs: Seq[ScheduledJob]) extends Message

  def apply(getJobs: JobList, e: JobExecutor, continue: (ScheduledJob, JobRun, MasterInfo) => Unit): Master = {
    val m = new MasterImpl(getJobs, e, continue)
    m.start()
    m
  }

  private class MasterImpl(getJobs: JobList, e: JobExecutor, continue: (ScheduledJob, JobRun, MasterInfo) => Unit) extends Master with LazyLogging {
    self =>
    private[this] val queue = new LinkedBlockingQueue[Message]()
    private[this] val state = new atomic.AtomicReference(MasterInfo.started)
    private[this] val shutdownLatch = new CountDownLatch(1)

    def start(): Unit = {
      if (!state.get.running) {
        logger.info("Starting master job …")
        val scheduler = Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
          def newThread(r: Runnable): Thread = {
            val t = Executors.defaultThreadFactory().newThread(r)
            t.setDaemon(true)
            t
          }
        })
        val schedule = scheduler.scheduleAtFixedRate(new Runnable() {
          def run: Unit = queue.offer(EvalTrigger)
        }, 1, 1, TimeUnit.MINUTES)

        Future {
          state.set(state.get.copy(running = true))
          logger.info("Master job running.")
          while (state.get.running) {
            queue.take() match {
              case Shutdown =>
                logger.info("Shutting down master job …")
                schedule.cancel(false)
                scheduler.shutdownNow()
                state.set(state.get.copy(running = false))
                scheduler.awaitTermination(10, TimeUnit.SECONDS)
                shutdownLatch.countDown()
              case msg =>
                state.set(self.run(msg, state.get))
            }
          }
        }
      }
    }

    def awaitTermination(wait: FiniteDuration): Unit =
      shutdownLatch.await(wait.toMillis, TimeUnit.MILLISECONDS)

    def info: MasterInfo = state.get
    def send(msg: Master.Message): Unit = queue.offer(msg)

    def activeJobs(now: LocalDateTime, state: MasterInfo): ScheduledJob => Boolean =
      sj => sj.config.shouldRun(now) && state.notRunning(sj)

    def run(msg: Message, state: MasterInfo): MasterInfo = {
      logger.trace(s"Got message $msg")
      msg match {
        case Toggle(active) =>
          state.copy(active = active)
        case JobDone(id) =>
          state.copy(runningJobs = state.runningJobs - id)
        case RunNow(jobs) =>
          val notRunning = jobs.filter(state.notRunning)
          logger.debug(s"Submitting ${notRunning.size} jobs")
          submit(notRunning)
          state.copy(runningJobs = state.runningJobs ++ notRunning.map(_.id))
        case EvalTrigger if state.active =>
          val now = LocalDateTime.now
          getJobs().map(_.filter(activeJobs(now, state))) match {
            case Right(jobs) =>
              logger.debug(s"Submitting ${jobs.size} jobs")
              submit(jobs)
              state.copy(runningJobs = state.runningJobs ++ jobs.map(_.id))
            case Left(error) =>
              logger.error("Error getting jobs", error)
              state
          }
        case _ =>
          state
      }
    }

    def submit(jobs: Seq[ScheduledJob]): Unit =
      Future {
        for (job <- jobs) {
          Try(e.exec(job))
            .flatMap(run => Try(continue(job, run, info)))
            .recover { case ex =>
              logger.error("Error running job", ex)
          }
          send(JobDone(job.id))
        }
      }
  }
}
