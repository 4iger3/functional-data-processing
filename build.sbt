ThisBuild / scalaVersion := "3.9.0"
ThisBuild / organization := "edu.functional"
ThisBuild / version := "0.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "typed-json",
    libraryDependencies ++= Seq(
      "com.monovore" %% "decline-effect" % "2.5.0",
      "com.typesafe" % "config" % "1.4.3",
      "co.fs2" %% "fs2-io" % "3.11.0",
      "io.circe" %% "circe-parser" % "0.14.10",
      "org.scalameta" %% "munit" % "1.0.4" % Test
    )
  )
