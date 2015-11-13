
//enablePlugins(ScalaJSPlugin)

scalaVersion := "2.11.7"

organization := "io.github.lptk"

version := "0.1-SNAPSHOT"


libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

libraryDependencies += "jline" % "jline" % "2.12"


//scalacOptions += "-deprecation"

scalacOptions += "-feature"

scalacOptions += "-language:postfixOps"

scalacOptions += "-language:postfixOps"

//scalacOptions ++= Seq("-Ypatmat-exhaust-depth", "40")


// Wanted nice Unicode (extended ASCII) characters when doing `sbt run`, but this messes up the REPL feedback beyond:

//fork in run := true

//connectInput in run := true

//javaOptions in run += "-Dfile.encoding=UTF-8"


//mainClass in (Compile, run) := Some("parsing.ParserREPL")
mainClass in (Compile, run) := Some("main.REPL")


// The following is because of a problem with sbt and jline (because sbt uses a different version?):
// after running for the first time, terminal input (arrow keys, tab, etc.) will not work properly, outputting things like ^[[D

fork in run := true

connectInput in run := true

outputStrategy := Some(StdoutOutput)




