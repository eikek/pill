package pill

import java.time._
import org.scalatest._
import pill.data._

class DataTest extends FlatSpec with Matchers {

  "timer" should "parse successfull" in {
    Timer.parse("Mon *-*-* 12:00") should be (Timer(List(DayOfWeek.MONDAY), Nil, Nil, Nil, List(12), List(0)))
    Timer.parse("2016-*-* 7:0") should be (Timer(Nil, List(2016), Nil, Nil, List(7), List(0)))
    Timer.parse("2016-*-* 7,8,12:0") should be (Timer(Nil, List(2016), Nil, Nil, List(7,8,12), List(0)))
  }

  it should "render correctly" in {
    Timer.parse("Mon,Wed *-*-* 12:0").asString should be ("Mon,Wed *-*-* 12:0")
    Timer.parse("2016-*-* 7:0").asString should be ("2016-*-* 7:0")
  }

  it should "calculate next trigger" in {
    val t1 = Timer.parse("2016-*-* 08:0,15")
    t1.nextTrigger(ldt(2016, 11, 22, 8, 10)) should be (Some(ldt(2016, 11, 22, 8, 15)))
    t1.nextTrigger(ldt(2016, 11, 22, 9, 10)) should be (Some(ldt(2016, 11, 23, 8, 0)))
    t1.nextTrigger(ldt(2016, 12, 31, 9, 30)) should be (None)

    val t2 = Timer.parse("*-10-* 8,10:0,15")
    t2.nextTrigger(ldt(2016,10,31,11,0)) should be (Some(ldt(2017,10,1,8,0)))

    val t3 = Timer.parse("Mon,Thu *-*-* 10:0")
    t3.nextTrigger(ldt(2016,11,26,21,50)) should be (Some(ldt(2016,11,28,10,0)))
    t3.nextTrigger(ldt(2016,11,28,10,15)) should be (Some(ldt(2016,12,1,10,0)))
  }

  private def ldt(y: Int, m: Int, d: Int, h: Int, min: Int): LocalDateTime =
    LocalDateTime.of(y, m, d, h, min)
}
