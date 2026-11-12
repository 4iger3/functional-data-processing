ThisBuild / scalaVersion := "3.9.0"
ThisBuild / organization := "edu.functional"
ThisBuild / version := "0.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "typed-csv",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.4" % Test
  )
