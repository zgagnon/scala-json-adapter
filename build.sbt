name := "scala-json-adapter"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.4"

scalariformSettings

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"


libraryDependencies ++= Seq(
  "io.spray" %% "spray-json" % "1.3.1",
  "org.json4s" %% "json4s-native" % "3.2.11",
  "org.scalaz" %% "scalaz-core" % "7.1.0",
  "org.specs2" %% "specs2" % "2.4.9" % "test",
  "com.typesafe.play" %% "play-json" % "2.3.6"
)

scalacOptions in Test ++= Seq("-Yrangepos")