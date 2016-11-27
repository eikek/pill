package pill

import java.net.InetAddress
import java.nio.file.{Path, Paths}
import java.util.{Locale, UUID}
import java.time._
import cats.syntax.either._
import pill.config._

package data {
  case class Id(id: String)
  
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
    year: List[Int], month: List[Int], day: List[Int],
    hour: List[Int], minute: List[Int]) {

    lazy val asString: String = {
      def str(l: List[Int]) = if (l.isEmpty) "*" else l.mkString(",")
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

    def nextTimers(startYear: Int): Stream[LocalDateTime] = {
      val comb = for {
        y <- if (year.isEmpty) Stream.from(startYear) else year.toStream
        m <- if (month.isEmpty) (1 to 12) else month
        d <- if (day.isEmpty) (1 to 31) else day
        h <- if (hour.isEmpty) (0 to 59) else hour
        min <- if (minute.isEmpty) (0 to 59) else minute
      } yield (y, m, d, h, min)
      //filter out invalid dates
      val dates = comb.flatMap({
        case (y,m,d,h,min) => Timer.localDateTime(y,m,d,h,min).toStream
      })
      if (dow.isEmpty) dates
      else dates.filter(ld => dow.contains(ld.getDayOfWeek))
    }

    def nextTrigger(ref: LocalDateTime): Option[LocalDateTime] = 
      nextTimers(ref.getYear).find(_ isAfter ref) 
  }

  object Timer {
    import fastparse.all._
    private object Parser {
      val empty: P[Seq[Int]] = P("*").map(_ => Nil)
      val dow: P[DayOfWeek] =
        P("Mon").map(_ => DayOfWeek.MONDAY) |
        P("Tue").map(_ => DayOfWeek.TUESDAY) |
        P("Wed").map(_ => DayOfWeek.WEDNESDAY) |
        P("Thu").map(_ => DayOfWeek.THURSDAY) |
        P("Fri").map(_ => DayOfWeek.FRIDAY) |
        P("Sat").map(_ => DayOfWeek.SATURDAY) |
        P("Sun").map(_ => DayOfWeek.SUNDAY)
  
      val dows: P[Seq[DayOfWeek]] = dow.rep(1, sep=",")
      val yearSingle: P[Int] = CharIn('0' to '9').rep(4).!.map(_.toInt)
      val years: P[Seq[Int]] = P(empty | yearSingle.rep(1, sep=","))
      val two: P[Int] = CharIn('0' to '9').rep(min=1, max=2).!.map(_.toInt)
      val twos: P[Seq[Int]] = P(empty | two.rep(1, sep=","))

      val offset: P[Duration] = P("+" ~ P(CharIn('0' to '9').rep.!).map(_.toInt) ~ P("min"|"h"|"d"|"m").!).map {
        case (num, unit) => unit.toLowerCase match {
          case "h" => Duration.ofHours(num.toLong)
          case "d" => Duration.ofDays(num.toLong)
          case "min" => Duration.ofMinutes(num.toLong)
          case _ => Duration.ofMinutes(num.toLong)
        }
      }
      val offsetTimer: P[Timer] = offset.map { duration =>
        val now = LocalDateTime.now.plus(duration)
        Timer(Nil, List(now.getYear),
          List(now.getMonthValue),
          List(now.getDayOfMonth),
          List(now.getHour),
          List(now.getMinute))
      }

      val stdTimer: P[Timer] = P((dows ~" ").? ~ years ~ "-" ~ twos ~"-"~ twos ~" "~ twos ~":"~ twos).map {
        case (w, y, m, d, h, min) => Timer(
          w.map(_.toList.sorted).getOrElse(Nil),
          y.toList.sorted, m.toList.sorted, d.toList.sorted,
          h.toList.sorted, min.toList.sorted)
      }
      val timer: P[Timer] = offsetTimer | stdTimer
    }

    def parse(s: String): Timer = P(Parser.timer ~ End).parse(s) match {
      case fastparse.core.Parsed.Success(c, _) => c
      case f => sys.error(f.toString)
    }

    def localDateTime(year: Int, month: Int, day: Int, hour: Int, minute: Int): Option[LocalDateTime] =
      Either.catchOnly[DateTimeException](LocalDateTime.of(year, month, day, hour, minute)).toOption

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
    nextRun: Instant = Instant.now,
    buildInfo: Map[String, String] = Map(
      "projectName" -> pill.config.BuildInfo.projectName,
      "version" -> pill.config.BuildInfo.version,
      "commit" -> pill.config.BuildInfo.commit,
      "buildTime" -> pill.config.BuildInfo.buildTime,
      "homepage" -> pill.config.BuildInfo.homepage.getOrElse("")
    ))
  {
    def notRunning(job: ScheduledJob): Boolean = !runningJobs.contains(job.id)
  }

  object MasterInfo {
    def localhostHostname = InetAddress.getLocalHost.getCanonicalHostName
    def masterName = Config.master.name match {
      case x if x.nonEmpty => x
      case _ => localhostHostname
    }
    lazy val started = MasterInfo(false, true, Instant.now, masterName, Set.empty)
  }
}
