package pill.build

import sbt._
import scala.util.{Try, Failure, Success}

object Util extends AutoPlugin {

  object autoImport {
    val genBuildInfo = taskKey[Seq[File]]("Generate a scala source file containing build information.")
  }

  def genBuildInfo(log: Logger, dir: File, version: String, basedir: File, name: String, homepage: Option[String]): Seq[File] = {
    log.info("Generate BuildInfo file ...")
    val code = s"""package pill.config
      |object BuildInfo {
      |  val version = "${version}"
      |  val buildTime = "${java.time.Instant.now}"
      |  val commit = "${findCurrentCommit(basedir)}"
      |  val projectName = "${name}"
      |  val homepage = ${homepage.map(s => "\""+s+"\"")}
      |}""".stripMargin
    val target = dir / "pill" / "config" / "BuildInfo.scala"
    IO.createDirectories(Seq(target.getParentFile))
    IO.write(target, code)
    Seq(target)
  }

  def currentCommitViaGit: Try[String] = {
    lazy val dirty = runGit(Seq("status", "--porcelain", "--untracked-files=no")).map {
      case "" => ""
      case _ => "dirty workingdir @ "
    }
    lazy val tag = runGit(Seq("name-rev", "--tags", "HEAD"), 2)
    lazy val branch = runGit(Seq("name-rev", "HEAD"), 2)
    for {
      d <- dirty
      n <- tag.filter(_ != "undefined").orElse(branch)
      h <- runGit(Seq("rev-parse", "--short=9", "HEAD"))
    } yield d + List(n, h).mkString("/")
  }

  def currentCommitViaFiles(basedir: File): Try[String] = Try {
    val List(_, refname) = IO.read(basedir / ".git" / "HEAD").split("\\s+", 2).toList
    val List(_, _, name) = refname.trim.split("/", 3).toList
    val hash = IO.read(basedir / ".git" / refname.trim).trim
    s"$name/${hash.substring(0, 9)}"
  }

  def findCurrentCommit(basedir: File): String = {
    val commit = currentCommitViaGit orElse currentCommitViaFiles(basedir)
    commit match {
      case Failure(ex) =>
        ex.printStackTrace()
        "[unknown commit]"
      case Success(info) => info
    }
  }
  def runGit(cmd: Seq[String], col: Int = 0): Try[String] = Try {
    val out = Process("git", cmd).!!.trim
    if (col > 0) out.split("\\s+", col).toList.last
    else out
  }

}
