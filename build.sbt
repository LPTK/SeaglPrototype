
enablePlugins(ScalaJSPlugin)

scalaVersion := "2.11.7"

organization := "io.github.lptk"

version := "0.1-SNAPSHOT"


libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"



// Wanted nice Unicode (extended ASCII) characters when doing `sbt run`, but this messes up the REPL feedback beyond:

//fork in run := true

//connectInput in run := true

//javaOptions in run += "-Dfile.encoding=UTF-8"





