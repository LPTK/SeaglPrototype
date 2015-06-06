package main

import parsing.SeaglParser._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import utils._

object Main extends App {

  println("Parsing...");
  val line = TermParser.program(new TermParser.lexical.Scanner(readLine()))
  
  println(line)

}
