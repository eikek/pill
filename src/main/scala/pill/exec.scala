package pill

import java.time._
import java.nio.file.Path
import scala.sys.process._
import cats.syntax.either._
import pill.data._, pill.files._

package exec {

  trait Executor {
    def exec(cmd: String, params: JobParams, silent: Boolean): Either[Exception, ExecResult]
  }
  object Executor extends Executor {

    def exec(cmd: String, params: JobParams, silent: Boolean) = {
      val stdout = new StringBuilder
      val stderr = new StringBuilder
      val logger =
        if (silent) ProcessLogger(line => (), line => ())
        else ProcessLogger(line => stdout.append(line).append("\n"), line => stderr.append(line).append("\n"))
      val proc = Process(cmd :: params.args.toList, params.cwd.map(_.toFile), params.env.toSeq: _*)
      val start = Instant.now
      Either.catchOnly[Exception] {
        val rc = proc ! logger
        ExecResult(rc, stdout.toString, stderr.toString, start, Duration.between(start, Instant.now), silent)
      }
    }
  }

  trait JobExecutor {
    def exec(job: ScheduledJob): JobRun
  }
  object JobExecutor {
    def apply(dir: Path, executor: Executor = Executor): JobExecutor =
      new JobExecutorImpl(dir, executor)
  }

  class JobExecutorImpl(dir: Path, executor: Executor = Executor) extends JobExecutor {

    def exec(job: ScheduledJob): JobRun = {
      val cmd = job.job match {
        case s:Script => Right(s)
        case Code(code, params) =>
          val p = (dir/job.id/s"run${job.id}")
          for {
            _ <- code >>: p
            _ <- p.mod755()
          } yield Script(p, params)
      }

      cmd.flatMap({ script =>
        val result = executor.exec(script.path.pathString, script.jobParams, job.config.silent)
        result.map(r => JobRun(job.id, r))
      }).valueOr(failureRun(job))
    }

    def failureRun(job: ScheduledJob)(ex: Exception): JobRun = {
      val sw = new java.io.StringWriter
      ex.printStackTrace(new java.io.PrintWriter(sw))
      JobRun(job.id, ExecResult(-1, "", sw.toString, Instant.now, Duration.ZERO, job.config.silent))
    }
  }
}
