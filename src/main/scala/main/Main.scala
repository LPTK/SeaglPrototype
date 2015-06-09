package main

import parsing.SeaglParser._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import utils._

object Main extends App {

  println("Parsing...");

  while (true) {
    print("> ")

    // Internal block because we don't want to type delimiters at top level
    val line = TermParser.internal_block(new TermParser.lexical.Scanner(readLine()))

    println(line)

    // TODO: better handling of the parsed value

  }

}
