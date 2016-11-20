package pill

import com.typesafe.scalalogging.LazyLogging
import pill.config._, pill.cli._

object Main extends LazyLogging {

  val cmds = List(
    new VersionCmd,
    new StartCmd,
    new StopCmd,
    new NewJobCmd,
    new JobDetailCmd,
    new JobListCmd,
    new DeleteJobCmd,
    new ChangeJobCmd,
    new ListRunsCmd,
    new LastRunCmd,
    new ShowRunCmd,
    new CleanRunsCmd,
    new MasterToggleCmd,
    new MasterInfoCmd
  )

  object HelpCmd extends Command[Unit]("help") {
    val description = ""
    val init = ()
    def run(u: Unit): Unit = {
      println(s"pill v${BuildInfo.version} (${BuildInfo.commit}) at ${BuildInfo.buildTime}")
      println()
      println("… a simple job scheduler. It will execute your scripts periodically.")
      println("It is controlled via REST and a basic command line interface is provided.")
      println()
      println("Commands:")
      println(cmds.map(j => s"${j.name}: ${j.description}").sorted.mkString("- ", "\n- ", "\n"))
      println("Each command can be asked for help using `--help'. The source and more")
      println(s"info is at ${BuildInfo.homepage.getOrElse("[oops, no homepage]")}.")
    }
  }

  def main(args: Array[String]): Unit = {
    logger.info("pill starting up …")
    logger.info(s"using config: ${Config.master}")

    args.toList match {
      case Nil =>
        println("Command required. Try `help'.")
      case cmd :: as =>
        (HelpCmd :: cmds).find(_.name == cmd) match {
          case Some(command) =>
            command.main(as)
          case None =>
            println(s"Command $cmd not found")
        }
    }
  }
}
