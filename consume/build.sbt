val dottyVersion = "0.19.0-RC1"

ThisBuild / name := "consume"
ThisBuild / version := "0.0.1"
ThisBuild / scalaVersion := dottyVersion

lazy val root = project
  .in(file("."))
  .settings(
    name := "consume-intent",
    organization := "com.factor10",
    libraryDependencies += "com.factor10" %% "intent" % "0.1.0",
    testFrameworks += new TestFramework("intent.sbt.Framework")
  )
