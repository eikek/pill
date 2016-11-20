package pill

import io.circe.syntax._
import pill.data._, pill.jsoncodec._

package show {

  trait Render[A]{
    def render(a: A): String
  }

  object Render {

    def apply[A](f: A => String): Render[A] = new Render[A] {
      def render(a: A) = f(a)
    }

    implicit val _jobRun: Render[JobRun] = Render[JobRun] { run =>
      val base =s"""${List.fill(80)('-').mkString}
         |Job: ${run.jobId}
         |Run: ${run.run}
         |Return code: ${run.result.returnCode}
         |Startet: ${run.result.started}
         |Runtime: ${run.result.runTime}
         |Silent: ${run.result.silent}""".stripMargin
      val stdout = if (run.result.stdout.nonEmpty) {
        s"""|\n${List.fill(37)('-').mkString}stdout${List.fill(37)('-').mkString}
            |${run.result.stdout}""".stripMargin
      } else ""
      val stderr = if (run.result.stderr.nonEmpty) {
        s"""|\n${List.fill(37)('-').mkString}stderr${List.fill(37)('-').mkString}
            |${run.result.stderr}""".stripMargin
      } else ""
      (base + stdout + stderr).trim
    }

    implicit val _scheduledJob: Render[ScheduledJob] = Render[ScheduledJob] { job => job.asJson.spaces2 }

    implicit val string = Render[String](identity)

    implicit def optRender[A](implicit r: Render[A]): Render[Option[A]] =
      Render[Option[A]] {
        case Some(a) => r.render(a)
        case _ => ""
      }

  }
}

package object show {

  def render[A](a: A)(implicit r:Render[A]): String = r.render(a)

  def show[A](a: A)(implicit r:Render[A]): Unit = println(render(a))
}
