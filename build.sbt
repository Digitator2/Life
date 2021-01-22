name := "Life"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"

mainClass in (Compile, run) := Some("LifeForm")