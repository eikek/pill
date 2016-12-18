import sbt._

object libs {

  val `scala-version` = "2.11.8"

  // https://github.com/melrief/pureconfig
  // MPL 2.0
  val pureconfig = "com.github.melrief" %% "pureconfig" % "0.4.0"

  // https://github.com/lihaoyi/fastparse
  // MIT
  val fastparse = "com.lihaoyi" %% "fastparse" % "0.4.2"

  // https://github.com/scopt/scopt
  // MIT
  val scopt = "com.github.scopt" %% "scopt" % "3.5.0"

  // https://github.com/typelevel/cats
  // MIT http://opensource.org/licenses/mit-license.php
  val `cats-core` = "org.typelevel" %% "cats-core" % "0.8.1"

  // https://github.com/finagle/finch
  // ASL 2.0
  val `finch-core` = "com.github.finagle" %% "finch-core" % "0.11.1"
  val `finch-circe` = "com.github.finagle" %% "finch-circe" % "0.11.1"

  // https://github.com/circe/circe
  // ASL 2.0
  val `circe-core` = "io.circe" %% "circe-core" % "0.6.1"
  val `circe-generic` = "io.circe" %% "circe-generic" % "0.6.1"
  val `circe-parser` = "io.circe" %% "circe-parser" % "0.6.1"

  // https://github.com/kohsuke/akuma
  // MIT
  val akuma = "org.kohsuke" % "akuma" % "1.10"

  // https://java.net/projects/javamail/pages/Home
  // CDDL 1.0, GPL 2.0
  val `javax-mail-api` = "javax.mail" % "javax.mail-api" % "1.5.6"
  val `javax-mail` = "com.sun.mail" % "javax.mail" % "1.5.6"

  // http://dnsjava.org/
  // BSD
  val dnsjava = "dnsjava" % "dnsjava" % "2.1.7" intransitive()

  // https://github.com/typesafehub/scala-logging
  // ASL 2.0
  val `scala-logging` = "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0" excludeAll(
    ExclusionRule("org.scala-lang", "scala-reflect"),
    ExclusionRule("org.scala-lang", "scala-library"),
    ExclusionRule("org.slf4j", "slf4j-api")
  )

  // http://logback.qos.ch/
  // EPL1.0 or LGPL 2.1
  val `logback-classic` = "ch.qos.logback" % "logback-classic" % "1.1.8"

  // https://github.com/scalaj/scalaj-http
  // ASL 2.0
  val `scalaj-http` = "org.scalaj" %% "scalaj-http" % "2.3.0"

  // https://github.com/lihaoyi/Ammonite
  // MIT
  val ammonite = "com.lihaoyi" % "ammonite" % "0.8.1"  cross CrossVersion.full

  // https://github.com/scalatest/scalatest
  // ASL 2.0
  val scalatest = "org.scalatest" %% "scalatest" % "3.0.1"

}
