name := "pill"
version := "0.0.1-SNAPSHOT"
scalaVersion := "2.11.8"
homepage := Option(url("https://github.com/eikek/pill"))

//resolvers += Resolver.sonatypeRepo("snapshots")
lazy val scalaLib = ExclusionRule("org.scala-lang", "scala-library")
lazy val slf4jApi = ExclusionRule("org.slf4j", "slf4j-api")

libraryDependencies ++= Seq(
  "com.github.melrief"   %% "pureconfig"    % "0.3.3",
  "com.lihaoyi"          %% "fastparse"     % "0.4.2",
  "com.github.scopt"     %% "scopt"         % "3.5.0",
  "org.typelevel"        %% "cats"          % "0.8.1",
//  "co.fs2"               %% "fs2-core"      % "0.9.2",
  "com.github.finagle"   %% "finch-core"    % "0.11.0-M4",
  "com.github.finagle"   %% "finch-circe"   % "0.11.0-M4",
  "io.circe"             %% "circe-core"    % "0.6.0",
  "io.circe"             %% "circe-generic" % "0.6.0",
  "io.circe"             %% "circe-parser"  % "0.6.0",
  "org.kohsuke"          %  "akuma"         % "1.10",
  "javax.mail"           %  "javax.mail-api" % "1.5.6",
  "com.sun.mail"         %  "javax.mail"    % "1.5.6",
  "dnsjava"              % "dnsjava"        % "2.1.6" intransitive(),
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0" excludeAll(
    ExclusionRule("org.scala-lang", "scala-reflect"),
    scalaLib,
    slf4jApi // use the one provided by logback
  ),
  "ch.qos.logback"       %  "logback-classic" % "1.1.7",
  "org.scalaj"           %% "scalaj-http"   % "2.3.0",
  "com.lihaoyi"          %  "ammonite"      % "0.8.0"  % "test" cross CrossVersion.full,
  "org.scalatest"        %% "scalatest"     % "3.0.1"  % "test",
  "org.scalacheck"       %% "scalacheck"    % "1.13.4" % "test"
)

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
