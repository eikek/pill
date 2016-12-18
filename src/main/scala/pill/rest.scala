package pill

import scala.concurrent.duration._
import com.typesafe.scalalogging.LazyLogging
import cats.syntax.either._
import io.finch._, io.finch.circe._
import io.circe._
import pill.data._, pill.jsoncodec._, pill.store._

package rest {

  object Api extends LazyLogging {

    def service(store: JobStore, master: Master, onShutdown: () => Unit) = {
      val endpoint = {
        jobList(store) :+: jobDetail(store) :+: jobParamsChange(store) :+:
        jobConfigChange(store, master) :+: jobRename(store) :+: jobDelete(store, master) :+:
        jobNew(store, master) :+: jobCreate(store, master) :+: jobExecute(store, master) :+:
        runDetail(store) :+: runList(store) :+: runDetailLatest(store) :+: runDelete(store) :+:
        runDeleteAll(store) :+: masterToggle(master) :+: masterInfo(master) :+:
        shutdown(master, onShutdown)
      }

      endpoint.toServiceAs[Application.Json]
    }


    implicit val encodeException: Encoder[Exception] = Encoder.instance(e => {
      logger.error("Error processing request", e)
      Json.obj(
        "success" -> Json.False,
        "type" -> Json.fromString(e.getClass.getSimpleName),
        "message" -> Json.fromString(e.getMessage)
      )
    })

    def postedJob: Endpoint[ScheduledJob] =
      jsonBody[String => ScheduledJob].map(_(ScheduledJob.makeId))

    def changedJobConf: Endpoint[JobConf => JobConf] =
      jsonBody[JobConf => JobConf]

    def changeParams: Endpoint[JobParams => JobParams] =
      jsonBody[JobParams => JobParams]

    def jobCreate(store: JobStore, master: Master): Endpoint[ScheduledJob] =
      post("api" :: "jobs" :: postedJob) { in: ScheduledJob =>
        store.save(in)
          .map(_ => Created(in))
          .map(reload(master))
          .valueOr(InternalServerError(_))
      }

    def reload[A](master: Master): A => A =
      a => { master.reload(); a }

    def jobNew(store: JobStore, master: Master): Endpoint[ScheduledJob] =
      post("api" :: "jobs" :: string :: jsonBody[String => ScheduledJob]) {
        (id: String, job: String => ScheduledJob) =>
        val j = job(id)
        store.create(j)
          .map(_ => Created(j))
          .map(reload(master))
          .valueOr(InternalServerError(_))
      }

    def jobDetail(store: JobStore): Endpoint[ScheduledJob] =
      get("api" :: "jobs" :: string) { id: String =>
        store.load(id)
          .map(j => j.map(Ok(_)).getOrElse(NotFound(new Exception(s"Job '$id' not found"))))
          .valueOr(InternalServerError(_))
      }

    def jobList(store: JobStore): Endpoint[Vector[ScheduledJob]] =
      get("api" :: "jobs") {
        store.list()
          .map(Ok(_))
          .valueOr(InternalServerError(_))
      }

    def jobParamsChange(store: JobStore): Endpoint[ScheduledJob] =
      put("api" :: "jobs" :: string :: "params" :: changeParams) {
        (id: String, change: JobParams => JobParams) =>

        store.load(id)
          .map(osj => osj.map(sj => sj.changeParams(change)))
          .flatMap({
            case Some(j) => store.save(j).map(_ => j)
            case None => Left(StoreError(s"Job $id not found"))
          })
          .map(Ok(_))
          .valueOr(InternalServerError(_))
      }

    def jobConfigChange(store: JobStore, master: Master): Endpoint[ScheduledJob] =
      put("api" :: "jobs" :: string :: "config" :: changedJobConf) {
        (id: String, change: JobConf => JobConf) =>

        store.load(id)
          .map(osj => osj.map(sj => sj.copy(config = change(sj.config))))
          .flatMap({
            case Some(j) => store.save(j).map(_ => j)
            case None => Left(StoreError(s"Job '$id' not found"))
          })
          .map(Ok(_))
          .map(reload(master))
          .valueOr(InternalServerError(_))
      }

    def jobRename(store: JobStore): Endpoint[ScheduledJob] =
      post("api" :: "jobs" :: string :: "rename" :: jsonBody[Id]) {
        (oldId: String, newId: Id) =>
        store.rename(oldId, newId.id)
          .flatMap(_ => store.load(newId.id))
          .map({
            case Some(j) => Ok(j)
            case None => InternalServerError(new Exception("Rename did not work"))
          })
          .valueOr(InternalServerError(_))
      }

    def jobChange(store: JobStore, master: Master): Endpoint[ScheduledJob] =
      put("api" :: "jobs" :: string :: jsonBody[ScheduledJob]) {
        (id: String, j: ScheduledJob) =>

        if (id == j.id) {
          store
            .save(j)
            .map(_ => Ok(j))
            .map(reload(master))
            .valueOr(InternalServerError(_))
        } else {
          BadRequest(new Exception("Job ids don't match"))
        }
      }

    def jobDelete(store: JobStore, master: Master): Endpoint[ScheduledJob] =
      delete("api" :: "jobs" :: string) { id: String =>
        store.delete(id)
          .map(Ok(_))
          .map(reload(master))
          .valueOr(InternalServerError(_))
      }

    def jobExecute(store: JobStore, master: Master): Endpoint[Json] =
      post("api" :: "jobs" :: string :: "execute") { id: String =>
        store.load(id)
          .map({
            case Some(job) =>
              master.send(Master.RunNow(Seq(job)))
              Ok(Json.obj("success" -> Json.True, "message" -> Json.fromString(s"Job $id executing.")))
            case None =>
              BadRequest(new Exception(s"Job $id not found"))
          })
          .valueOr(InternalServerError(_))
      }

    def runDetail(store: JobStore): Endpoint[JobRun] =
      get("api" :: "jobs" :: string :: "runs" :: int) { (id: String, run: Int) =>
        store.loadRun(id, run)
          .map(j => j.map(Ok(_)).getOrElse(NotFound(new Exception(s"Job run of job '$id' and number '$run' not found"))))
          .valueOr(InternalServerError(_))
      }

    def runDetailLatest(store: JobStore): Endpoint[JobRun] =
      get("api" :: "jobs" :: string :: "runs" :: "latest") { id: String =>
        store.loadLatestRun(id)
          .map(j => j.map(Ok(_)).getOrElse(NotFound(new Exception(s"Latest run run of job '$id' not found"))))
          .valueOr(InternalServerError(_))
      }

    def runList(store: JobStore): Endpoint[Vector[JobRun]] =
      get("api" :: "jobs" :: string :: "runs") { id: String =>
        store.listRuns(id).map(Ok(_)).valueOr(InternalServerError(_))
      }

    def runDelete(store: JobStore): Endpoint[JobRun] =
      delete("api" :: "jobs" :: string :: "runs" :: int) { (id: String, run: Int) =>
        store.deleteRun(id, run) .map(Ok(_)) .valueOr(InternalServerError(_))
      }

    def runDeleteAll(store: JobStore): Endpoint[Map[String, Int]] =
      delete("api" :: "jobs" :: string :: "runs" :: paramOption("keep").as[Int]) { (id: String, keep: Option[Int]) =>
        store.deleteAllRuns(id, keep)
          .map(n => Ok(Map("deleted" -> n)))
          .valueOr(InternalServerError(_))
      }

    def masterToggle(m: Master): Endpoint[MasterToggle] =
      put("api" :: "master" :: jsonBody[MasterToggle]) { toggle: MasterToggle =>
        val old = MasterToggle(m.info.active)
        m.toggle(toggle.active)
        Ok(old)
      }

    def masterInfo(m: Master): Endpoint[MasterInfo] =
      get("api" :: "master")(Ok(m.info))

    def shutdown(m: Master, onShutdown: () => Unit): Endpoint[Map[String, Boolean]] =
      post("api" :: "shutdown") {
        onShutdown()
        m.shutdown()
        m.awaitTermination(30.seconds)
        Ok(Map("shutdown" -> true))
      }
  }
}
