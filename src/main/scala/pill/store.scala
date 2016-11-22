package pill

import java.nio.file.Path
import cats.syntax.either._, cats.syntax.option._, cats.instances.vector._, cats.instances.int._
import io.circe._, io.circe.syntax._, io.circe.parser.decode
import pill.data._, pill.files._, pill.jsoncodec._

package store {

  trait StoreError extends Exception
  object StoreError {
    def apply(msg: String): StoreError = new Exception(msg) with StoreError
    def fromException(ex: Exception): StoreError = {
      val error = StoreError(ex.getClass +": "+ ex.getMessage)
      error.setStackTrace(ex.getStackTrace)
      error
    }
  }

  trait JobStore {

    type Result[A] = Either[Exception, A]
    object Result {
      final def error[A](msg: String): Result[A] = Left(StoreError(msg))
      final def error[A](ex: Exception): Result[A] = Left(StoreError.fromException(ex))
    }
    def create(job: ScheduledJob): Result[Unit]
    def save(job: ScheduledJob): Result[Unit]
    def load(id: String): Result[Option[ScheduledJob]]
    def delete(id: String): Result[ScheduledJob]
    def list(): Result[Vector[ScheduledJob]]

    def saveRun(run: JobRun): Result[JobRun]
    def loadRun(id: String, run: Int): Result[Option[JobRun]]
    def loadLatestRun(id: String): Result[Option[JobRun]]
    def listRuns(id: String): Result[Vector[JobRun]]
    def deleteRun(id: String, run: Int): Result[JobRun]
    def deleteAllRuns(id: String, keep: Option[Int]): Result[Int]
  }

  object JobStore {
    def directory(dir: Path): JobStore = {
      mkdir(dir) match {
        case Right(_) => new FileStore(dir)
        case Left(err) => throw err
      }
    }
  }

  private class FileStore(root: Path) extends JobStore {
    import Result._ 
    private val jobFile = "job.json"

    private def loadFile[A](p: Path)(implicit d: Decoder[A]) =
      p.contents.flatMap(decode[A]).map(_.some)

    def create(job: ScheduledJob): Result[Unit] = {
      val target = root / job.id / jobFile
      for {
        _ <- mkdir(root)
        _ <- mkdirAtomic(target.parent)
        _ <- job.asJson.spaces2 >>: target
      } yield ()
    }

    def save(job: ScheduledJob): Result[Unit] = {
      val target = root / job.id / jobFile
      for {
        _ <- mkdir(target.parent)
        _ <- job.asJson.spaces2 >>: target
      } yield ()
    }

    def load(id: String): Result[Option[ScheduledJob]] = {
      val target = root / id / jobFile
      if (target.notExists) Right(None)
      else loadFile[ScheduledJob](target)
    }

    def delete(id: String): Result[ScheduledJob] = {
      val target = root / id 
      load(id).flatMap {
        case Some(j) => target.rmR().map(_ => j)
        case None => error(s"Job with id '$id' does not exist")
      }
    }

    def list(): Result[Vector[ScheduledJob]] = {
      mkdir(root)
      root.list(child => (child / jobFile).fileExists)
        .map(ps => ps.map(_ / jobFile).map(loadFile[ScheduledJob]))
        .map(_.map(_.map(_.toVector)))
        .flatMap(elist => elist.fold(Right(Vector()))(_ combine _))
    }


    def nextRunCounter(id: String): Result[Int] = {
      val file = root/id/"runs"/"counter"
      val count =
        if (file.notExists) Right(0)
        else file.contents.flatMap(str => Either.catchOnly[Exception](str.toInt))

      for {
        _ <- mkdir(file.parent)
        c <- count
        _ <- (c + 1).toString >>: file
      } yield c + 1
    }

    def saveRun(run: JobRun): Result[JobRun] = {
      val jobRun = nextRunCounter(run.jobId).map(n => run.copy(run = n))

      def doSave(jr: JobRun) = {
        val target = root/jr.jobId/"runs"/jr.run
        if (target.exists) error(s"Job run '${jr.run}' of job '${jr.jobId}' already exists")
        else for {
          _ <- mkdir(target.parent)
          _ <- jr.asJson.spaces2 >>: target
        } yield target
      }

      for {
        jr <- jobRun
        _  <- doSave(jr)
      } yield jr
    }

    def loadRun(id: String, run: Int): Result[Option[JobRun]] = {
      val target = root / id / "runs" / run
      if (target.notExists) Right(None)
      else loadFile[JobRun](target)
    }

    def listRunFiles(id: String) = {
      val runs = root/id/"runs"
      if (runs.notExists) Right(Vector())
      else runs.list(child => child.isFile && child.name.matches("\\d+"))
    }

    def listRunFilesOrdered(id: String) =
      listRunFiles(id).map(_.sortBy(_.name).reverse)

    def loadLatestRun(id: String): Result[Option[JobRun]] =
      listRunFilesOrdered(id).map(_.take(1))
        .map(_.headOption.map(loadFile[JobRun]))
        .flatMap({
          case Some(e) => e
          case None => Right(None)
        })

    def listRuns(id: String): Result[Vector[JobRun]] = {
      listRunFiles(id)
        .map(ps => ps.map(loadFile[JobRun]))
        .map(_.map(_.map(_.toVector)))
        .flatMap(elist => elist.fold(Right(Vector()))(_ combine _))
    }

    def deleteRun(id: String, run: Int): Result[JobRun] = {
      val target = root/id/"runs"/run
      loadRun(id, run).flatMap {
        case Some(jr) =>
          target.rmR().map(_ => jr)
        case None =>
          error(s"Run '$run' of job '$id' does not exist")
      }
    }

    def deleteAllRuns(id: String, keep: Option[Int]): Result[Int] = {
      listRunFilesOrdered(id)
        .map(_.drop(keep getOrElse 0))
        .map(_.map(p => p.rmR().map(_ => 1)))
        .flatMap(elist => elist.fold(Right(0))(_ combine _))
    }
  }
}
