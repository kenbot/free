scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.0-M5", 
  "org.scala-lang" % "scala-swing" % "2.10.4", 
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
  "org.scalatest" %% "scalatest" % "2.1.3" % "test") 

fork := true

fork in Test := false

initialCommands := """
  import kenbot.free.tank.maths._;
  import kenbot.free.tank.ai._;
  import kenbot.free.tank.app._;
  import kenbot.free.tank.model._"""
