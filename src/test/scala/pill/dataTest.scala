package pill

import java.time._
import org.scalatest._
import pill.data._

class DataTest extends FlatSpec with Matchers {

  "timer" should "parse successfull" in {
    Timer.parse("Mon *-*-* 12:00") should be (Timer(List(DayOfWeek.MONDAY), Nil, Nil, Nil, List("12"), List("00")))
    Timer.parse("2016-*-* 7:0") should be (Timer(Nil, List("2016"), Nil, Nil, List("7"), List("0")))
  }

  it should "render correctly" in {
    Timer.parse("Mon,Wed *-*-* 12:00").asString should be ("Mon,Wed *-*-* 12:00")
    Timer.parse("2016-*-* 7:0").asString should be ("2016-*-* 7:0")
  }

}
