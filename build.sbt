name := "scala-json-adapter"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  "io.spray" %% "spray-json" % "1.3.1",
  "org.specs2" %% "specs2" % "2.4.9" % "test"
)