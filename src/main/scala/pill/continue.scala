package pill

import com.typesafe.scalalogging.{LazyLogging, Logger}
import cats.data._, cats.data.Validated.{Valid, Invalid}
import cats.syntax.either._
import pill.store._, pill.data._
import pill.show._, pill.mail._, pill.config._

package continue {

  case class Continue(f: Cont => Cont) {
    def andThen(c: Continue): Continue = Continue(f compose c.f)
    def ==>(c: Continue): Continue = andThen(c)
    def last(c: Continue): Cont = andThen(c).run
    def ==>|(c: Continue): Cont = last(c)
    def run = f(Continue.none)
  }

  object Continue {
    val none : Cont = (_, _, _) => ()
    def apply(f: Cont): Continue = Continue(c => (job, run, info) => {
      f(job, run, info)
      c(job, run, info)
    })
  }

  object instances extends LazyLogging {

    def saveRun(store: JobStore): Continue = Continue(c => (job, run, info) => {
      logger.debug("Store run information")
      store.saveRun(run) match {
        case Right(nr) => c(job, nr, info)
        case Left(err) =>
          logger.error("Error saving run!", err)
          c(job, run, info)
      }
    })

    def handleRunOnce(store: JobStore): Continue = Continue((job, run, _) => {
      logger.debug("Update job properties")
      if (job.config.runOnce) {
        val inactive = job.copy(config = job.config.copy(active = false))
        store.save(inactive)
          .logError(logger, s"Error deactivating 'runOnce' job ${job.id}")
      }
    })

    def sendNotificationMail(smtp: SmtpClient): Continue = Continue((job, run, info) => {
      val rcpt =
        if (run.result.returnCode != 0) job.config.errorMails ++ job.config.mails
        else job.config.mails
      logger.debug(s"Sending notification mails to ${rcpt.size} recipients")
      if (rcpt.nonEmpty) {
        val text = "Hello,\n\nthe following job has been run:\n" +
        render(job) +
        "\n\nThe result was:\n" +
        render(run)
        val subject = s"Run ${run.run} of job ${job.id} on ${info.name}"
        smtp.send(Message(rcpt, Config.master.smtp.mailfrom, subject, text))
          .logError(logger, "Error sending mail")
      }
    })

    def handleKeepRuns(store: JobStore): Continue = Continue((job, run, _) => {
      job.config.keepRuns match {
        case Some(n) =>
          logger.debug(s"Deleting runs for job ${job.id}, keeping $n")
          store.deleteAllRuns(job.id, Some(n))
            .map(n => logger.debug(s"Deleted $n runs for job ${job.id}"))
            .logError(logger, "Unable to delete run information")
        case _ =>
      }
    })
  }
}
package object continue {
  type Cont = (ScheduledJob, JobRun, MasterInfo) => Unit

  implicit class EitherLogOps[A](ee: Either[Exception, Unit]) {
    def logError(logger: Logger, msg: => String): Unit =
      ee match {
        case Right(_) =>
        case Left(ex) => logger.error(msg, ex)
      }
  }

  implicit class ValidatedNelLogOps[A](ee: ValidatedNel[Exception, Unit]) {
    def logError(logger: Logger, msg: => String): Unit =
      ee match {
        case Invalid(errors) =>
          errors.toList.foreach(ex => logger.error(msg, ex))
        case Valid(_) =>
      }
  }
}
