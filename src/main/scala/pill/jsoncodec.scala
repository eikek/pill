package pill

import java.nio.file.{Path, Paths}
import java.time._
import cats.syntax.either._
import io.circe._, io.circe.generic.semiauto._
import pill.data._

object jsoncodec {
  implicit val _idDecoder: Decoder[Id] = deriveDecoder[Id]
  implicit val _idEncoder: Encoder[Id] = deriveEncoder[Id]

  implicit val _pathDecoder: Decoder[Path] = Decoder.decodeString.map(s => Paths.get(s))
  implicit val _pathEncoder: Encoder[Path] = Encoder.encodeString.contramap[Path](_.toString)

  implicit val _instantDecoder: Decoder[Instant] = Decoder.decodeString.map(s => Instant.parse(s))
  implicit val _instantEncoder: Encoder[Instant] = Encoder.encodeString.contramap[Instant](_.toString)

  implicit val _durationDecoder: Decoder[Duration] = Decoder.decodeString.map(s => Duration.parse(s))
  implicit val _durationEncoder: Encoder[Duration] = Encoder.encodeString.contramap[Duration](_.toString)

  implicit val _mailDecoder: Decoder[Mail] = deriveDecoder[Mail]
  implicit val _mailEncoder: Encoder[Mail] = deriveEncoder[Mail]

  implicit val _timerEncoder: Encoder[Timer] = Encoder.encodeString.contramap[Timer](_.asString)
  implicit val _timerDecoder: Decoder[Timer] = Decoder.decodeString.emap { str =>
    Either.catchNonFatal(Timer.parse(str)).leftMap(t => "Timer")
  }

  implicit val _jobParamsDecoder: Decoder[JobParams] = deriveDecoder[JobParams]
  implicit val _jobParamsEncoder: Encoder[JobParams] = deriveEncoder[JobParams]
  implicit val _jobParamsDec: Decoder[JobParams => JobParams] = deriveDecoder[JobParams => JobParams]

  implicit val _jobconfDecoder: Decoder[JobConf] = deriveDecoder[JobConf]
  implicit val _jobconfEncoder: ObjectEncoder[JobConf] = deriveEncoder[JobConf]
  implicit val _jobconfDec: Decoder[JobConf => JobConf] = deriveDecoder[JobConf => JobConf]

  // not using derived en/decoders for Job, because the json looks a bit ugly:
  // "job" : {
  //   "Script" : {
  //     "path" : "/home/backup.sh"
  //   }
  // }
  // as opposed to
  // "job" : {
  //   "path" : "/home/backup.sh"
  // }
  val scriptDecoder: Decoder[Job] = Decoder.forProduct2[Path, Option[JobParams], Job]("path", "params")(Script.apply)
  val codeDecoder: Decoder[Job] = Decoder.forProduct2("code", "params")(Code.apply)
  implicit val _jobDecoder: Decoder[Job] = scriptDecoder.or(codeDecoder)

  val scriptEncoder: Encoder[Script] = deriveEncoder[Script]
  val codeEncoder: Encoder[Code] = deriveEncoder[Code]
  implicit val _jobEncoder: Encoder[Job] = new Encoder[Job] {
    def apply(job: Job): Json = job match {
      case s: Script => scriptEncoder(s)
      case c: Code => codeEncoder(c)
    }
  }

  implicit val _scheduledJobDecoder: Decoder[ScheduledJob] = deriveDecoder[ScheduledJob]
  implicit val _scheduledJobEncoder: Encoder[ScheduledJob] = deriveEncoder[ScheduledJob]

  implicit val _scheduledJobDecEnc1 = deriveDecoder[ScheduledJob => ScheduledJob]
  implicit val _scheduledJobDecEnc2 = deriveDecoder[String => ScheduledJob]

  implicit val _execResultDecoder: Decoder[ExecResult] = deriveDecoder[ExecResult]
  implicit val _execResultEncoder: Encoder[ExecResult] = deriveEncoder[ExecResult]

  implicit val _runDecoder: Decoder[JobRun] = deriveDecoder[JobRun]
  implicit val _runEncoder: Encoder[JobRun] = deriveEncoder[JobRun]

  implicit val _masterToggleDecoder: Decoder[MasterToggle] = deriveDecoder[MasterToggle]
  implicit val _masterToggleEncoder: Encoder[MasterToggle] = deriveEncoder[MasterToggle]

  implicit val _masterInfoDecoder: Decoder[MasterInfo] = deriveDecoder[MasterInfo]
  implicit val _masterInfoEncoder: Encoder[MasterInfo] = deriveEncoder[MasterInfo]
  
}
