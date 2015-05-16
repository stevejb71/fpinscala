name := "FPInScala"

version := "1.0"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:_")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"

initialCommands in console := """import fpinscala._, Traverse._"""