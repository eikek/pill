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

  def reload(): Unit = send(Master.EvalTrigger)
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

    implicit val ldtOrd = new Ordering[LocalDateTime] {
      def compare(x: LocalDateTime, y: LocalDateTime): Int =
        if (x.isBefore(y)) -1 else if (x.isAfter(y)) 1 else 0
    }

    def start(): Unit = {
      if (!state.get.running) {
        logger.info("Starting master job …")
        val scheduler = Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
          def newThread(r: Runnable): Thread = {
            val t = Executors.defaultThreadFactory().newThread(r)
            t.setDaemon(true)
            t.setName("pill-scheduler")
            t
          }
        })
        def schedule(msg: Message, at: Instant): ScheduledFuture[Message] = {
          val delay = math.max(500, at.toEpochMilli - Instant.now.toEpochMilli)
          logger.debug(s"Scheduling next run on: $at (in ${delay}ms)")
          scheduler.schedule(new Callable[Message]() {
            def call(): Message = {
              queue.offer(msg)
              msg
            }
          }, delay, TimeUnit.MILLISECONDS)
        }

        Future {
          state.set(state.get.copy(running = true))
          logger.info("Master job running.")
          var next = schedule(EvalTrigger, state.get.nextRun)
          while (state.get.running) {
            queue.take() match {
              case Shutdown =>
                logger.info("Shutting down master job …")
                next.cancel(true)
                scheduler.shutdownNow()
                state.set(state.get.copy(running = false))
                scheduler.awaitTermination(10, TimeUnit.SECONDS)
                shutdownLatch.countDown()
              case msg =>
                Either.catchOnly[Exception](self.run(msg, state.get)) match {
                  case Right(nextState) =>
                    if (state.get.nextRun != nextState.nextRun) {
                      next.cancel(true)
                      next = schedule(EvalTrigger, nextState.nextRun)
                    }
                    state.set(nextState)
                  case Left(err) =>
                    logger.error("Internal error in `run'", err)
                }
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
      logger.trace(s"Got message $msg (active=${state.active})")
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
          getJobs() match {
            case Right(jobs) =>
              val submitJobs = jobs.filter(activeJobs(now, state))
              logger.debug(s"Submitting ${submitJobs.size} jobs")
              submit(submitJobs)
              jobs.flatMap(_.config.timer.nextTrigger(now).toList) match {
                case ts if ts.isEmpty =>
                  state.copy(runningJobs = state.runningJobs ++ submitJobs.map(_.id))
                case ts =>
                  val nextRun = ts.min.atZone(ZoneId.systemDefault).toInstant
                  state.copy(runningJobs = state.runningJobs ++ submitJobs.map(_.id), nextRun = nextRun)
              }
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
