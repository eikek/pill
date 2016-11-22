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
        jobList(store) :+: jobDetail(store) :+: jobParamsChange(store) :+: jobRename(store) :+:
        jobDelete(store) :+: jobNew(store) :+: jobCreate(store) :+: runDetail(store) :+:
        runList(store) :+: runDetailLatest(store) :+: runDelete(store)   :+:
        runDeleteAll(store) :+: masterToggle(master) :+: masterInfo(master) :+:
        shutdown(master, onShutdown) :+: jobConfigChange(store)
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
      body.as[String => ScheduledJob].map(_(ScheduledJob.makeId))

    def changedJobConf: Endpoint[JobConf => JobConf] =
      body.as[JobConf => JobConf]

    def changeParams: Endpoint[JobParams => JobParams] =
      body.as[JobParams => JobParams]

    def jobCreate(store: JobStore): Endpoint[ScheduledJob] =
      post("api" :: "jobs" :: postedJob) { in: ScheduledJob =>
        store.save(in)
          .map(_ => Created(in))
          .valueOr(InternalServerError(_))
      }

    def jobNew(store: JobStore): Endpoint[ScheduledJob] =
      post("api" :: "jobs" :: string :: body.as[String => ScheduledJob]) {
        (id: String, job: String => ScheduledJob) =>
        val j = job(id)
        store.create(j)
          .map(_ => Created(j))
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

    def jobConfigChange(store: JobStore): Endpoint[ScheduledJob] =
      put("api" :: "jobs" :: string :: "config" :: changedJobConf) {
        (id: String, change: JobConf => JobConf) =>

        store.load(id)
          .map(osj => osj.map(sj => sj.copy(config = change(sj.config))))
          .flatMap({
            case Some(j) => store.save(j).map(_ => j)
            case None => Left(StoreError(s"Job '$id' not found"))
          })
          .map(Ok(_))
          .valueOr(InternalServerError(_))
      }

    def jobRename(store: JobStore): Endpoint[ScheduledJob] =
      post("api" :: "jobs" :: string :: "rename" :: body.as[Id]) {
        (oldId: String, newId: Id) =>
        store.rename(oldId, newId.id)
          .flatMap(_ => store.load(newId.id))
          .map({
            case Some(j) => Ok(j)
            case None => InternalServerError(new Exception("Rename did not work"))
          })
          .valueOr(InternalServerError(_))
      }

    def jobChange(store: JobStore): Endpoint[ScheduledJob] =
      put("api" :: "jobs" :: string :: body.as[ScheduledJob]) {
        (id: String, j: ScheduledJob) =>

        if (id == j.id) {
          store.save(j).map(_ => Ok(j)).valueOr(InternalServerError(_))
        } else {
          BadRequest(new Exception("Job ids don't match"))
        }
      }

    def jobDelete(store: JobStore): Endpoint[ScheduledJob] =
      delete("api" :: "jobs" :: string) { id: String =>
        store.delete(id)
          .map(Ok(_))
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
      put("api" :: "master" :: body.as[MasterToggle]) { toggle: MasterToggle =>
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
