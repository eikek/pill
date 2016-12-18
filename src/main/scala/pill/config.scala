package pill

import scala.util.Try
import java.nio.file.{Path, Paths}
import pureconfig._, pureconfig.ConfigConvert._
import cats.syntax.either._
import pill.files._, pill.data.Mail

package config {

  case class SmtpSettings(host: String, port: Int, user: String, password: String, mailfrom: Mail)

  case class HttpSettings(bindHost: String, port: Int) {
    def isBound = Ports.used(bindHost, port)
    def isNotBound = !isBound
    val hostAndPort = s"$bindHost:$port"
  }

  case class MasterConfig(dir: Path, name: String, smtp: SmtpSettings, http: HttpSettings) {
    val endpointFile = dir/"endpoint"
    def endpointFromFile: Either[Exception, Option[HttpSettings]] =
      Option(endpointFile)
        .filter(_.exists)
        .map(_.contents.flatMap(Ports.splitHostAndPort))
        .map(_.map { case (host, port) => Some(HttpSettings(host, port)) })
        .getOrElse(Right(None))

    def newEndpoint: Either[Exception, HttpSettings] = {
      val port = if (http.port == 0) Ports.generate(http.bindHost) else http.port
      val newHttp = HttpSettings(http.bindHost, port)
      for {
        _ <- mkdir(endpointFile.parent)
        _ <- (newHttp.hostAndPort >>: endpointFile)
      } yield newHttp
    }

    def httpHostAndPortUnbound = endpointFromFile
      .flatMap(_.filter(_.isNotBound).map(Right(_)).getOrElse(newEndpoint))

  }

  case class LogConfig(level: String, file: Path)

  case class CliConfig(endpointFile: Path, endpoint: String, protocol: String) {
    def endpointFromFile: Either[Exception, Option[String]] =
      if (endpoint.nonEmpty) Right(Some(endpoint))
      else Option(endpointFile)
        .filter(_.exists)
        .map(_.contents.flatMap(Ports.splitHostAndPort))
        .map(_.map { case (host, port) => Some(protocol+"://"+host+":"+port) })
        .getOrElse(Right(None))
  }

  object Config {
    implicit private val _pathConvert = stringConvert[Path](
      s => Try(Paths.get(s).absolute),
      p => p.toString)
    implicit private val _mailConvert = stringConvert[Mail](
      s => Try(Mail(s)),
      mail => mail.address)

    val defaultConfigFile = home/".config"/"pill"/"pill.conf"

    val configFile: Option[Path] =
      Option(System.getProperty("config.file"))
        .filter(_.nonEmpty)
        .map(s => Paths.get(s))
        .orElse(Some(defaultConfigFile))
        .filter(_.exists)

    val master: MasterConfig = configFile
      .map(p => loadConfig[MasterConfig](p, "pill.master"))
      .getOrElse(loadConfig[MasterConfig]("pill.master")).get

    val log: LogConfig = configFile
      .map(p => loadConfig[LogConfig](p, "pill.log"))
      .getOrElse(loadConfig[LogConfig]("pill.log")).get

    val cli: CliConfig = configFile
      .map(p => loadConfig[CliConfig](p, "pill.cli"))
      .getOrElse(loadConfig[CliConfig]("pill.cli")).get
  }

  object Ports {
    val range = (2000, 25000)

    def generate(host: String): Int = nextUnused(range, host)

    private def randomInt(range: (Int, Int)): Int =
      ((math.random * (range._2 - range._1)) + range._1).toInt

    def nextUnused(range: (Int, Int), host: String, count: Int = 0): Int = {
      if (count > 1000) sys.error("cannot find unused port")
      val next = randomInt(range)
      if (unused(host, next)) next else nextUnused(range, host, count + 1)
    }

    def unused(host: String, port: Int) = !used(host, port)
    def used(host: String, port: Int) = {
      val s = new java.net.Socket()
      val bound = Either.catchOnly[Exception] {
        s.connect(new java.net.InetSocketAddress(host, port))
        s.isBound
      }
      s.close
      bound.valueOr(_ => false)
    }

    private[config] def splitHostAndPort(s: String): Either[Exception, (String, Int)] =
      s.lastIndexOf(':') match {
        case idx if idx > 0 =>
          Either.catchOnly[Exception]{
            (s.substring(0, idx), s.substring(idx+1).toInt)
          }
        case _ => Left(new Exception(s"'$s' is not in 'hostname:port' format"))
      }
  }
}
