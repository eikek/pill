package pill

import java.net.InetAddress
import java.nio.file.{Path, Paths}
import java.util.{Locale, UUID}
import java.time._
import pill.config._

package data {

  case class JobParams(
    args: Seq[String] = Seq.empty,
    env: Map[String, String] = Map.empty,
    cwd: Option[Path] = None
  )

  sealed trait Job 
  case class Script(path: Path, params: Option[JobParams]) extends Job {
    val jobParams = params.getOrElse(JobParams())
  }
  object Script {
    def apply(path: String, params: Option[JobParams]): Script = Script(Paths.get(path), params)
  }

  case class Code(code: String, params: Option[JobParams]) extends Job {
    val jobParams = params.getOrElse(JobParams())
  }

  case class Mail(address: String) {
    val (user, domain) = {
      address.lastIndexOf('@') match {
        case idx if idx > 0 =>
          (address.substring(0, idx), address.substring(idx+1))
        case _ => sys.error("Invalid email: "+ address)
      }
    }
  }

  // Thu,Fri 2012-*-1,5 11:12:13
  case class Timer(dow: List[DayOfWeek],
    year: List[String], month: List[String], day: List[String],
    hour: List[String], minute: List[String]) {

    lazy val asString: String = {
      def str(l: List[String]) = if (l.isEmpty) "*" else l.mkString(",")
      val days =
        if (dow.isEmpty) ""
        else dow.map(_.getDisplayName(format.TextStyle.SHORT, Locale.ROOT)).mkString(",") + " "
      days + str(year) +"-"+ str(month) +"-"+ str(day) +" "+ str(hour) +":"+ str(minute)
    }

    def triggeredNow = triggered(LocalDateTime.now)

    def triggered(now: LocalDateTime): Boolean = {
      def check[A](values: List[A], needle: A): Boolean =
        values.isEmpty || values.contains(needle)

      check(dow, now.getDayOfWeek) && check(year.map(_.toInt), now.getYear) &&
      check(month.map(m => Month.of(m.toInt)), now.getMonth) &&
      check(day.map(_.toInt), now.getDayOfMonth) &&
      check(hour.map(_.toInt), now.getHour) &&
      check(minute.map(_.toInt), now.getMinute)
    }
  }

  object Timer {
    import fastparse.all._
    private object Parser {
      val empty: P[Seq[String]] = P("*").map(_ => Nil)
      val dow: P[DayOfWeek] =
        P("Mon").map(_ => DayOfWeek.MONDAY) |
        P("Tue").map(_ => DayOfWeek.TUESDAY) |
        P("Wed").map(_ => DayOfWeek.WEDNESDAY) |
        P("Thu").map(_ => DayOfWeek.THURSDAY) |
        P("Fri").map(_ => DayOfWeek.FRIDAY) |
        P("Sat").map(_ => DayOfWeek.SATURDAY) |
        P("Sun").map(_ => DayOfWeek.SUNDAY)
  
      val dows: P[Seq[DayOfWeek]] = dow.rep(1, sep=",")
      val yearSingle: P[String] = CharIn('0' to '9').rep(4).!
      val years: P[Seq[String]] = P(empty | yearSingle.rep(1, sep=","))
      val two: P[String] = CharIn('0' to '9').rep(min=1, max=2).!
      val twos: P[Seq[String]] = P(empty | two.rep(1, sep=","))

      val timer: P[Timer] = P((dows ~" ").? ~ years ~ "-" ~ twos ~"-"~ twos ~" "~ twos ~":"~ twos).map {
        case (w, y, m, d, h, min) => Timer(w.map(_.toList).getOrElse(Nil), y.toList, m.toList, d.toList, h.toList, min.toList)
      }
    }

    def parse(s: String): Timer = P(Parser.timer ~ End).parse(s) match {
      case fastparse.core.Parsed.Success(c, _) => c
      case f => sys.error(f.toString)
    }

    lazy val always = Timer.parse("*-*-* *:*")
  }

  case class JobConf(
    timer: Timer,
    mails: Seq[Mail] = Seq.empty,
    errorMails: Seq[Mail] = Seq.empty,
    runOnce: Boolean = false,
    silent: Boolean = false,
    active: Boolean = true,
    keepRuns: Option[Int] = None) {

    def shouldRun(now: LocalDateTime): Boolean =
      active && timer.triggered(now)
  }

  case class ScheduledJob(id: String, job: Job, config: JobConf) {
    def changeParams(change: JobParams => JobParams): ScheduledJob =
      job match {
        case Script(p, params) => copy(job = Script(p, Some(change(params.getOrElse(JobParams())))))
        case Code(s, params) => copy(job = Code(s, Some(change(params.getOrElse(JobParams())))))
      }

    def params: Option[JobParams] = job match {
      case Script(_, params) => params
      case Code(_, params) => params
    }
  }

  object ScheduledJob {
    def makeId = UUID.randomUUID().toString.substring(0, 6)
    def apply(job: Job, config: JobConf): ScheduledJob = 
      ScheduledJob(makeId, job, config)
  }

  case class ExecResult(
    returnCode: Int,
    stdout: String,
    stderr: String,
    started: Instant,
    runTime: Duration,
    silent: Boolean
  )

  case class JobRun(
    jobId: String,
    run: Int,
    result: ExecResult
  )
  object JobRun {
    def apply(jobId: String, result: ExecResult): JobRun = JobRun(jobId, -1, result)
  }

  case class MasterToggle(active: Boolean)

  case class MasterInfo(
    running: Boolean,
    active: Boolean,
    started: Instant,
    name: String,
    runningJobs: Set[String],
    buildInfo: Map[String, String] = Map(
      "projectName" -> pill.config.BuildInfo.projectName,
      "version" -> pill.config.BuildInfo.version,
      "commit" -> pill.config.BuildInfo.commit,
      "buildTime" -> pill.config.BuildInfo.buildTime,
      "homepage" -> pill.config.BuildInfo.homepage.getOrElse("")
    )
  )

  object MasterInfo {
    def localhostHostname = InetAddress.getLocalHost.getCanonicalHostName
    def masterName = Config.master.name match {
      case x if x.nonEmpty => x
      case _ => localhostHostname
    }
    lazy val started = MasterInfo(false, true, Instant.now, masterName, Set.empty)
  }
}
