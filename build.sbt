name := "Checkers"

version := "0.1"

scalaVersion := "2.13.8"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1"

lazy val myProject = (project in file(".")).settings(
  assembly / assemblyOutputPath := file("./Checkers.jar")
)