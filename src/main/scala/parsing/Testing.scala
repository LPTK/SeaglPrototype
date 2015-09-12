package parsing

import scala.io.StdIn._


//object QuickTest extends App {println(Parser.parse("a"))}

object QuickTest extends App {println(Parser.parse("a => b | c => d"))}

//object QuickTest extends App {println(Parser.parse("""
//a
//  b
//    c
//    d
//  e
//"""))}

//object QuickTest extends App {println(Parser.parse("""
//a
//"""))}

//object QuickTest extends App {println(Parser.parse("""a"""))}



object ParserREPL extends App {
  
  println("Parsing...");
  
  while (true) {
    print("> ")
  
    val line = readLine()
  
    // Internal block because we don't want to type delimiters at top level
    val pgrm = Parser.pgrm(new Parser.lexical.Scanner(line))
  
    println(pgrm)
  
    // TODO: better handling of the parsed value
  
  }
  
}
  

