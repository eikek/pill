import libs._

name := "pill"
version := "0.0.1-SNAPSHOT"
scalaVersion := `scala-version`
homepage := Option(url("https://github.com/eikek/pill"))

libraryDependencies ++= Seq(
  pureconfig, fastparse, scopt, `cats-core`,
  `finch-core`, `finch-circe`, `scalaj-http`,
  `circe-core`, `circe-generic`, `circe-parser`,
  akuma, `javax-mail-api`, `javax-mail`, dnsjava,
  `scala-logging`, `logback-classic`) ++
   Seq(ammonite, scalatest).map(_ % "test")

scalacOptions ++= Seq(
  "-encoding", "UTF-8",
//  "-Xfatal-warnings", // fail when there are warnings
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused-import"
)

genBuildInfo := pill.build.Util.genBuildInfo(
  streams.value.log,
  sourceManaged.value,
  version.value,
  (baseDirectory in Compile).value,
  name.value,
  homepage.value.map(_.toString))
sourceGenerators in Compile += genBuildInfo.taskValue


initialCommands in (Test, console) := """ammonite.Main().run()"""
fork in run := true
//javaOptions in run ++= Seq(s"-Dpill.master.dir=${target.value}")
mainClass in Compile := Some("pill.Main")
