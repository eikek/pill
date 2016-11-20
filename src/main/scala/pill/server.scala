package pill

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference
import com.twitter.util.Await
import com.twitter.finagle.{Http, ListeningServer}
import com.typesafe.scalalogging.LazyLogging
import cats.syntax.either._
import pill.store._
import pill.config._, pill.files._, pill.mail._
import pill.exec._, pill.continue.instances._

object Server extends LazyLogging {
  def main(args: Array[String]): Unit = {
    logger.info("Starting server via pill.Server")
    val http = Config.master.endpointFromFile
      .flatMap(_.map(Right(_)).getOrElse(Config.master.newEndpoint))
      .valueOr(ex => throw ex)
    val p = implicitly[cli.OptParser[HttpSettings]]
    p.parser("none").parse(args, http) match {
      case Some(cfg) =>

      case None =>
    }

    start(http)
  }

  def restApi(mainServer: AtomicReference[ListeningServer]) = {
    val store = createStore(Config.master.dir/"store")
    val master = createMaster(Config.master.dir/"store", store)
    rest.Api.service(store, master,
      () => Option(mainServer.get).map(server => server.close()))
  }

  def createStore(dir: Path): JobStore = JobStore.directory(dir)

  def createMaster(dir: Path, store: JobStore): Master = {
    val smtp = SmtpClient.fromConfig
    val cont = saveRun(store) ==> handleRunOnce(store) ==> sendNotificationMail(smtp) ==>| handleKeepRuns(store)
    Master(store.list, JobExecutor(dir), cont)
  }

  def start(cfg: HttpSettings): Boolean = {
    if (cfg.isBound) {
      false
    } else {
      val mainServer = new AtomicReference[ListeningServer]
      val api = restApi(mainServer)
      val server = Http.server
        .withLabel("pill")
        .serve(cfg.hostAndPort, api)
      mainServer.set(server)
      logger.info(s"Started http server at ${cfg.hostAndPort}")
      Await.ready(server)
      logger.info("Http server shut down.")
      true
    }
  }
}
