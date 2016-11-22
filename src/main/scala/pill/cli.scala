package pill

import java.nio.file.{Path, Paths}
import scala.sys.process._
import com.sun.akuma._
import com.typesafe.scalalogging.LazyLogging
import scalaj.http._
import cats.syntax.either._
import scopt._
import io.circe.Json, io.circe.syntax._, io.circe.parser.decode
import pill.data._, pill.config._, pill.jsoncodec._
import pill.files._, pill.show._

package object cli {

  def httpApi(path: String): HttpRequest = {
    val endpoint = Config.cli.endpointFromFile.map(_.get).valueOr(ex => throw ex)
    Http(s"$endpoint/api$path")
  }

  def printJsonBody(resp: HttpResponse[String]): Unit = {
    decode[Json](resp.body) match {
      case Right(j) => println(j.spaces2)
      case Left(err) =>
        println("Body: "+ resp.body)
        throw err
    }
  }

  def whenOk(resp: HttpResponse[String])(body: => Any): Unit = {
    if (resp.code == 200 || resp.code == 201) {
      body
    } else {
      println(s"${resp.code} Failed!")
      printJsonBody(resp)
    }
  }
}

package cli {
  case class JobChange(id: String, change: ScheduledJob => ScheduledJob)
  case class CleanRuns(id: String, keep: Int = 10)
  case class RunSelect(id: String, run: Int = 1)
  case class Rename(from: String, to: String)

  abstract class Command[C](val name: String)(implicit p: OptParser[C]) {
    def init: C
    def description: String
    def before(): Unit = ()
    def run(cfg: C): Unit
    final def main(args: Seq[String]): Unit = {
      val parser = p.parser(name)
      if (description.nonEmpty) {
        parser.head("\n" + description + "\n")
      }
      parser.parse(args, init) match {
        case Some(cfg) =>
          before()
          run(cfg)
        case _ =>
      }
    }
    abstract class Parser extends OptionParser[C](name) {
      note(s"$name ${BuildInfo.version}")
      help("help") text ("print this help")
    }
  }
  trait RunningServer {
    self: Command[_] =>
    override def before(): Unit = ensureRunning()
    def ensureRunning(): Boolean = {
      val cfg = Config.master.endpointFromFile
        .flatMap(_.map(Right(_)).getOrElse(Config.master.newEndpoint))
        .valueOr(ex => throw ex)

      if (cfg.isBound) false
      else {
        val classes = classOf[StartCmd]
          .getClassLoader.asInstanceOf[java.net.URLClassLoader]
          .getURLs.toList
          .mkString(java.io.File.pathSeparator)

        val args = Seq("java", "-cp", classes, pill.Main.getClass.getName.dropRight(1), "start")
        Process(args).!
        Thread.sleep(800) // todo find some better way
        true
      }
    }
  }

  class VersionCmd extends Command[Unit]("version") {
    val description = "Show the version of the cli client"
    val init = ()
    def run(s: Unit) = {
      println(s"pill v${BuildInfo.version} (${BuildInfo.commit}) at ${BuildInfo.buildTime}")
    }
  }

  class StartCmd extends Command[HttpSettings]("start") with LazyLogging {
    val description = "Starts the master scheduler as daemon in background"
    def init = Config.master.endpointFromFile
      .flatMap(_.map(Right(_)).getOrElse(Config.master.newEndpoint))
      .valueOr(ex => throw ex)

    def run(cfg: HttpSettings) = {
      if (!cfg.isBound) {
        val d = new Daemon()
        if (d.isDaemonized) {
          d.init()
        } else {
          println(s"Starting server at ${cfg.hostAndPort}")
          d.daemonize()
          System.exit(0)
        }
        Server.start(cfg)
      } else {
        println(s"Server already running at ${cfg.hostAndPort}")
      }
    }
  }

  class StopCmd extends Command[HttpSettings]("stop") {
    val description = "Stop the master scheduler daemon"
    def init = Config.master.endpointFromFile
      .valueOr(ex => throw ex)
      .getOrElse(HttpSettings("", -1))
    def run(http: HttpSettings): Unit = {
      if (http.port > 0 && http.isBound) {
        Either.catchOnly[Exception] {
          println("Stopping server …")
          val resp = httpApi(s"/shutdown").postData("").asString
          whenOk(resp)(printJsonBody(resp))
        }
      } else {
        println(s"Endpoint ${http.hostAndPort} not up")
      }
    }
  }

  class JobListCmd extends Command[Unit]("list") with RunningServer {
    val description = "List all scheduled jobs"
    val init = ()
    def run(u: Unit): Unit = {
      val resp = httpApi(s"/jobs").asString
      whenOk(resp) {
        decode[Vector[ScheduledJob]](resp.body) match {
          case Right(jobs) =>
            for (j <- jobs) {
              val name = j.job match {
                case Script(path, _) => path.toString
                case _:Code => "<code>"
              }
              println(s"${j.id}\t ${j.config.timer.asString}\t$name")
            }
          case Left(err) =>
            throw err
        }
      }
    }
  }

  class JobDetailCmd extends Command[Id]("show") with RunningServer {
    val description = "Show details of a scheduled job"
    val init = Id("")
    def run(job: Id): Unit = {
      val resp = httpApi(s"/jobs/${job.id}").asString
      whenOk(resp) {
        decode[ScheduledJob](resp.body) match {
          case Right(sj) =>
            println(render(sj))
          case Left(err) =>
            throw err
        }
      }
    }
  }

  class NewJobCmd extends Command[ScheduledJob]("new") with RunningServer {
    val description = "Schedule a new job"
    val initTimer = Timer.parse("2016-11-19 19:00")
    val init = ScheduledJob("", Code("", None), JobConf(initTimer))
    def run(job: ScheduledJob): Unit = {
      val resp = (if (job.id == "") httpApi("/jobs") else httpApi(s"/jobs/${job.id}"))
        .postData(job.asJson.noSpaces)
        .header("content-type", "application/json")
        .asString
      whenOk(resp) {
        decode[ScheduledJob](resp.body) match {
          case Right(j) =>
            println(s"Created ${j.id} running at ${j.config.timer.asString}.")
          case Left(err) =>
            println("Created")
            throw err
        }
      }
    }
  }

  class RenameJobCmd extends Command[Rename]("rename") with RunningServer {
    val description = "Rename a job"
    val init = Rename("", "")
    def run(rename: Rename): Unit = {
      val resp = httpApi(s"/jobs/${rename.from}/rename")
        .postData(Id(rename.to).asJson.noSpaces)
        .header("content-type", "application/json")
        .asString
      whenOk(resp) {
        println(s"Job ${rename.from} is now ${rename.to}")
      }
    }
  }

  class DeleteJobCmd extends Command[Id]("rm") with RunningServer {
    val description = "Delete a scheduled job"
    val init = Id("")
    def run(id: Id): Unit = {
      val resp = httpApi(s"/jobs/${id.id}").method("DELETE").asString
      whenOk(resp) {
        println(s"Deleted job ${id.id}")
      }
    }
  }

  class ChangeJobCmd extends Command[JobChange]("change") with RunningServer {
    val description = "Change properties of a scheduled job"
    val init: JobChange = JobChange("", identity)
    def run(cfg: JobChange): Unit = {
      val get = httpApi(s"/jobs/${cfg.id}").asString
      whenOk(get) {
        decode[ScheduledJob](get.body) match {
          case Right(sj) =>
            val nj = cfg.change(sj)
            if (nj.config != sj.config) {
              val resp = httpApi(s"/jobs/${cfg.id}/config")
                .postData(nj.config.asJson.noSpaces)
                .method("PUT")
                .header("content-type", "application/json")
                .asString
              whenOk(resp) {
                println("Updated job config.")
              }
            }
            if (nj.params != sj.params) {
              nj.params.foreach { params =>
                val resp = httpApi(s"/jobs/${cfg.id}/params")
                  .postData(params.asJson.noSpaces)
                  .method("PUT")
                  .header("content-type", "application/json")
                  .asString
                whenOk(resp) {
                  println("Updated job params.")
                }
              }
            }
          case Left(err) =>
            throw err
        }
      }
    }
  }

  class ListRunsCmd extends Command[Id]("runs") with RunningServer {
    val description = "List the runs of a job"
    val init = Id("")
    def run(id: Id): Unit = {
      val resp = httpApi(s"/jobs/${id.id}/runs").asString
      whenOk(resp) {
        decode[Vector[JobRun]](resp.body) match {
          case Right(runs) =>
            for (j <- runs.sortBy(-_.run)) {
              println(f"${j.jobId}\t ${j.run}%5d\t rc=${j.result.returnCode}%3d\t runtime=${j.result.runTime}")
            }
          case Left(err) =>
            throw err
        }
      }
    }
  }

  class CleanRunsCmd extends Command[CleanRuns]("clean-runs") with RunningServer {
    val description = "Free disk space by removing old runs"
    val init = CleanRuns("")
    def run(cr: CleanRuns): Unit = {
      val resp = httpApi(s"/jobs/${cr.id}/runs")
        .method("DELETE")
        .param("keep", cr.keep.toString)
        .asString
      whenOk(resp)(printJsonBody(resp))
    }
  }

  class LastRunCmd extends Command[Id]("last") with RunningServer {
    val description = "Show details about last run"
    val init = Id("")
    def run(id: Id): Unit = {
      val resp = httpApi(s"/jobs/${id.id}/runs/latest").asString
      whenOk(resp) {
        decode[JobRun](resp.body) match {
          case Right(run) =>
            println(render(run))
          case Left(err) =>
            throw err
        }
      }
    }
  }

  class ShowRunCmd extends Command[RunSelect]("run") with RunningServer {
    val description = "Show details about any run"
    val init = RunSelect("")
    def run(select: RunSelect): Unit = {
      val resp = httpApi(s"/jobs/${select.id}/runs/${select.run}").asString
      whenOk(resp) {
        decode[JobRun](resp.body) match {
          case Right(run) =>
            println(render(run))
          case Left(err) =>
            throw err
        }
      }
    }
  }

  class MasterToggleCmd extends Command[MasterToggle]("pause") with RunningServer {
    val description = "Pause the master scheduler"
    val init = MasterToggle(true)
    def run(p: MasterToggle): Unit = {
      val resp = httpApi("/master")
        .postData(p.asJson.noSpaces)
        .method("PUT")
        .header("content-type", "application/json")
        .asString
      whenOk(resp)(printJsonBody(resp))
    }
  }

  class MasterInfoCmd extends Command[Unit]("info") {
    val description = "Show some info about the master scheduler"
    val init = ()
    def run(u: Unit): Unit = {
      println(s"Endpoint: ${httpApi("").url}")
      val resp = httpApi("/master").asString
      whenOk(resp)(printJsonBody(resp))
    }
  }

  trait OptParser[A] {
    def parser(name: String): OptionParser[A]
  }
  object OptParser {
    def apply[A](p: String => OptionParser[A]): OptParser[A] = new OptParser[A] {
      def parser(name: String) = p(name)
    }

    implicit val _timerRead: Read[Timer] = Read.reads(Timer.parse)
    implicit val _mailRead: Read[Mail] = Read.reads(Mail.apply)
    implicit val _pathRead: Read[Path] = Read.reads(s => Paths.get(s).absolute)

    class Parser[C](name: String) extends OptionParser[C](name) {
      head(s"${BuildInfo.projectName} v${BuildInfo.version}")
      help("help") text ("Print this help")

      def timerOpt = opt[Timer]('t', "timer") text("The timer definition")
      def mailsOpt = opt[Seq[Mail]]('m', "mail") text(
        "Mail addresses to notify with job output")
      def addMailOpt = opt[Seq[Mail]]("add-mail") text ("Add another mail address")
      def rmMailOpt = opt[Seq[Mail]]("rm-mail") text ("Remove a mail address from notification")
      def errorMailsOpt = opt[Seq[Mail]]("error-mail") abbr("em") text(
        "Mail addresses to notify about error output only")
      def addErrorMailOpt = opt[Seq[Mail]]("add-error-mail") text ("Add another mail address to be notified on error")
      def rmErrorMailOpt = opt[Seq[Mail]]("rm-error-mail") text ("Remove a mail address from notification on error")
      def onceOptB = opt[Boolean]("once") text ("Set the `once' flag of a job")
      def silentOptB = opt[Boolean]("silent") text ("Set the `silent' flag of a job")
      def activeOptB = opt[Boolean]("active") text ("Set the `active' flag on a job")
      def nRunOpt = opt[Int]('n', "run") required() text ("Select the run to show")

      def workingDirOpt = opt[Path]("wd") text("The job's working directory")
      def envOpt = opt[Map[String, String]]("env") text ("Environment variables used for the job")

      def portOpt = opt[Int]('p', "port") text("The port to use")
      def hostOpt = opt[String]('h', "host") text("The host to bind")

      def programArg = arg[Path]("<program>") required() text ("The script/program to run (required)")
      def argsArg = arg[String]("<args...>").unbounded().optional().text("Arguments to the script")
      def argsOpt = opt[Seq[String]]("args") text ("The new arguments for the job")

      def keepOpt = opt[Int]("keep") text("How many of the last runs to keep")
      def idArg = arg[String]("<id>").required().text("The job id (required)")
      def idOpt = opt[String]("id") text("The job id to use. A random one is generated, if omitted")
      def pauseOpt = opt[Boolean]("active") required() text("Pause the master scheduler")

      def fromArg = arg[String]("<from>") required() text ("The job id to rename")
      def toArg = arg[String]("<to>") required() text ("The new job id")
    }

    implicit val _id: OptParser[Id] = OptParser[Id](n => new Parser[Id](n) {
      note("\nArguments:")
      idArg action { (id,cfg) => Id(id) }
    })

    implicit val _unit: OptParser[Unit] = OptParser[Unit](_ => new Parser(""))

    implicit val _httpSettings: OptParser[HttpSettings] = OptParser[HttpSettings] {
      name => new Parser[HttpSettings](name) {
        portOpt action { (p, cfg) => cfg.copy(port = p) }
        hostOpt action { (h, cfg) => cfg.copy(bindHost = h) }
      }
    }

    implicit val _rename: OptParser[Rename] = OptParser[Rename] {
      name => new Parser[Rename](name) {
        fromArg action { (id, cfg) => cfg.copy(from = id) }
        toArg action { (id, cfg) => cfg.copy(to = id) }
      }
    }

    implicit val _runSelect: OptParser[RunSelect] = OptParser[RunSelect] {
      name => new Parser[RunSelect](name) {
        nRunOpt action { (n, cfg) => cfg.copy(run = n) }
        note("\nArguments")
        idArg required() action { (id, cfg) => cfg.copy(id = id) }
      }
    }

    implicit val _masterToggle: OptParser[MasterToggle] = OptParser[MasterToggle] {
      name => new Parser[MasterToggle](name) {
        pauseOpt action { (f, cfg) => cfg.copy(active = f) }
      }
    }

    implicit val _cleanRuns: OptParser[CleanRuns] = OptParser[CleanRuns] {
      name => new Parser[CleanRuns](name) {
        keepOpt action { (k, cfg) => cfg.copy(keep = k) }
        idArg required() action { (id, cfg) => cfg.copy(id = id) }
      }
    }

    implicit val _jobChange: OptParser[JobChange] = OptParser[JobChange] {
      name => new Parser[JobChange](name) {
        timerOpt action { (t, cfg) =>
          cfg.copy(change = cfg.change andThen { sj => sj.copy(config = sj.config.copy(timer = t)) })
        }
        mailsOpt action { (m, cfg) =>
          cfg.copy(change = cfg.change andThen { cfg => cfg.copy(config = cfg.config.copy(mails = m)) })
        }
        addMailOpt action { (m, cfg) =>
          cfg.copy(change = cfg.change andThen { cfg => cfg.copy(config = cfg.config.copy(mails = cfg.config.mails ++ m)) })
        }
        rmMailOpt action { (m, cfg) =>
           cfg.copy(change = cfg.change andThen { cfg => cfg.copy(config = cfg.config.copy(mails = cfg.config.mails diff m)) })
        }
        errorMailsOpt action { (m, cfg) =>
          cfg.copy(change = cfg.change andThen { cfg => cfg.copy(config = cfg.config.copy(errorMails = m)) })
        }
        addErrorMailOpt action { (m, cfg) =>
           cfg.copy(change = cfg.change andThen { cfg => cfg.copy(config = cfg.config.copy(errorMails = cfg.config.errorMails ++ m)) })
        }
        rmErrorMailOpt action { (m, cfg) =>
           cfg.copy(change = cfg.change andThen { cfg => cfg.copy(config = cfg.config.copy(errorMails = cfg.config.errorMails diff m)) })
        }
        onceOptB action { (b, cfg) =>
          cfg.copy(change = cfg.change andThen { cfg => cfg.copy(config = cfg.config.copy(runOnce = b)) })
        }
        silentOptB action { (b, cfg) =>
          cfg.copy(change = cfg.change andThen { cfg => cfg.copy(config = cfg.config.copy(silent = b)) })
        }
        activeOptB action { (b, cfg) =>
          cfg.copy(change = cfg.change andThen { cfg => cfg.copy(config = cfg.config.copy(active = b)) })
        }
        keepOpt action { (n, cfg) =>
          cfg.copy(change = cfg.change andThen { cfg => cfg.copy(config = cfg.config.copy(keepRuns = Some(n))) })
        }

        workingDirOpt action { (p, cfg) =>
          def update: Option[JobParams] => Option[JobParams] =
            params => params.map(jp => jp.copy(cwd = Some(p))).orElse(Some(JobParams(cwd = Some(p))))
          cfg.copy(change = cfg.change andThen { cfg =>
            cfg.job match {
              case Script(p, params) =>
                cfg.copy(job = Script(p, update(params)))
              case Code(s, params) =>
                cfg.copy(job = Code(s, update(params)))
            }
          })
        }

        argsOpt action { (arg, cfg) =>
          def update: Option[JobParams] => Option[JobParams] =
            params => params.map(jp => jp.copy(args = arg)).orElse(Some(JobParams(args = arg)))

          cfg.copy(change = cfg.change andThen { cfg =>
            cfg.job match {
              case Script(p, params) =>
                cfg.copy(job = Script(p, update(params)))
              case Code(p, params) =>
                cfg.copy(job = Script(p, update(params)))
            }
          })
        }
        note("\nArguments:")
        idArg action { (id, cfg) =>
          cfg.copy(id = id)
        }
      }
    }

    implicit val _scheduledJob: OptParser[ScheduledJob] = OptParser[ScheduledJob] {
      name => new Parser[ScheduledJob](name) {
        timerOpt required() action { (t, cfg) =>
          cfg.copy(config = cfg.config.copy(timer = t))
        }
        mailsOpt action { (m, cfg) =>
          cfg.copy(config = cfg.config.copy(mails = m))
        }
        errorMailsOpt action { (m, cfg) =>
          cfg.copy(config = cfg.config.copy(errorMails = m))
        }
        onceOptB action { (b, cfg) =>
          cfg.copy(config = cfg.config.copy(runOnce = b))
        }
        silentOptB action { (b, cfg) =>
          cfg.copy(config = cfg.config.copy(silent = b))
        }
        activeOptB action { (b, cfg) =>
          cfg.copy(config = cfg.config.copy(active = b))
        }
        keepOpt action { (n, cfg) =>
          cfg.copy(config = cfg.config.copy(keepRuns = Some(n)))
        }
        idOpt action { (id, cfg) => cfg.copy(id = id) }


        workingDirOpt action { (p, cfg) =>
          def update: Option[JobParams] => Option[JobParams] =
            params => params.map(jp => jp.copy(cwd = Some(p))).orElse(Some(JobParams(cwd = Some(p))))
          cfg.job match {
            case Script(p, params) =>
              cfg.copy(job = Script(p, update(params)))
            case Code(s, params) =>
              cfg.copy(job = Code(s, update(params)))
          }
        }

        note("\nArguments:")
        programArg action { (p, cfg) =>
          cfg.job match {
            case Script(_, params) =>
              cfg.copy(job = Script(p, params))
            case Code(_, params) =>
              cfg.copy(job = Script(p, params))
          }
        }
        argsArg action { (arg, cfg) =>
          def update: Option[JobParams] => Option[JobParams] =
            params => params.map(jp => jp.copy(args = jp.args :+ arg)).orElse(Some(JobParams(args = Seq(arg))))

          cfg.job match {
            case Script(p, params) =>
              cfg.copy(job = Script(p, update(params)))
            case Code(p, params) =>
              cfg.copy(job = Script(p, update(params)))
          }
        }
      }
    }
  }
}
