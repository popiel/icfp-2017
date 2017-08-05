name := """lambda-punter"""
organization := "com.wolfskeep.icfp2017"

version := "1.1-SNAPSHOT"

scalaVersion := "2.12.2"

enablePlugins(JavaAppPackaging)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.3"

// scalacOptions += "-P:artima-supersafe:config-file:project/supersafe.cfg"
