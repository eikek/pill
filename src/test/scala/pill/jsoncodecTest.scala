package pill

import org.scalatest._
import pill.data._, pill.jsoncodec._
import io.circe._, io.circe.syntax._, io.circe.parser.decode

class JsonCodecTest extends FlatSpec with Matchers {

  def dec[A](json: String)(implicit d: Decoder[A]): A = decode[A](json) match {
    case Right(a) => a
    case Left(err) => throw err //sys.error(msg)
  }

  "scheduledJob" should "encode to json" in {
    val jc = JobConf(Timer.parse("2016-*-* 12:30"))
    val sj1 = ScheduledJob(Script("/home/backup.sh", None), jc)
    info(sj1.asJson.spaces2)
    dec[ScheduledJob](sj1.asJson.noSpaces) should be (sj1)

    val sj2 = ScheduledJob(Code("#!/usr/bin/env bash\n\necho 'hello'", None), jc)
    info(sj2.asJson.spaces2)
    dec[ScheduledJob](sj2.asJson.spaces2) should be (sj2)
  }

  it should "decode from json" in {
    val json1 = """{
     |  "id" : "9e9ee5",
     |  "job" : {
     |    "path" : "/home/backup.sh",
     |    "params" : null
     |  },
     |  "config" : {
     |    "timer" : "2016-*-* 12:30",
     |    "mails" : [
     |    ],
     |    "errorMails" : [
     |    ],
     |    "runOnce" : false,
     |    "silent" : false,
     |    "active" : true,
     |    "keepRuns" : null
     |  }
     |}""".stripMargin

    val json2 = """{
     |  "id" : "9e9ee5",
     |  "job" : {
     |    "code" : "#!/usr/bin/env bash\n\necho 'hello'",
     |    "params" : null
     |  },
     |  "config" : {
     |    "timer" : "2016-*-* 12:30",
     |    "mails" : [
     |    ],
     |    "errorMails" : [
     |    ],
     |    "runOnce" : false,
     |    "silent" : false,
     |    "active" : true,
     |    "keepRuns" : null
     |  }
     |}""".stripMargin

    dec[ScheduledJob](json1).asJson.spaces2 should be (json1)
    dec[ScheduledJob](json2).asJson.spaces2 should be (json2)
  }
}
