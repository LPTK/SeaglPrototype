package main

import java.io.PrintWriter

import common.Stages2.Typed
import front2._
import utils._  
import common._

import jline._
import console._
import parsing.Parser

import scala.util.parsing.input.Position

import completer._

/**
  * Nice, Colored REPL for Seagl.
  * 
  * TODO: make it work better in IntelliJ
  * Adding the following helps: -Djline.terminal=jline.UnsupportedTerminal
  * But still, indentation is broken (`reader.getCursorBuffer.write` does not seem to work)
  * 
  * Box drawing:
  * https://en.wikipedia.org/wiki/Box-drawing_character
  * 
  * TODO: maybe enter multi-line mode if first line entered is empty?
  * 
  * Complex type mismatch messages should be printed this way:
  *   Type mismatch:
  *   Type 1: ...
  *   Type 2: ...
  *   Type 1 was inferred here: ...
  *     because: ...
  *   Type 2 was inferred here: ...
  *     because: ...
  * 
  */
object REPL extends ConsoleReader {
  val reader = this  
	
  reader.addCompleter(new StringsCompleter("foo", "bar", "baz", "lol", "lololol"))
  
  val out = new PrintWriter(reader.getOutput)
  
  import Console._
  
  val Ls(first, pre, post) = Ls("┌ ", "│ ", "└> ") map (_ in CYAN)
  
  val GREY = "\u001B[37m" // Light Grey
  
  implicit class ClrStr(str: Str) {
    def in(clr: Str) = clr + str + RESET
  }
  def info(str: Str, clr: Str = GREY) = s"$BOLD$clr$str: $RESET"
  
  def main(args: Array[String]): Unit = {
    
    val pr = out.println(_: Any)  
    
    //println(("a").length.toString)
    //println(("a" in BLUE) map (_ toInt) mkString " ")
    
    //val frameChar = "~o~" in CYAN
    ////val frameChar = ("~"+("o" in BOLD)+"~") in CYAN
    //val frame = frameChar * 11 // 26 chars
    //pr(frame)
    //pr( frameChar + " Welcome to  " + ("Leopar" in RED in BOLD) + (s" v.$VERSION  " in YELLOW) + frameChar )
    //pr(frame)
    
    val frameLine = "═" * 29
    val frameChar = "║" in CYAN
    val cutie = "~" in BLUE in BOLD
    val txtClr = RESET //YELLOW
    pr(("╔" + frameLine + "╗") in CYAN)
    pr( s"$frameChar $cutie " + ("Welcome to " in txtClr) + ("Leopar" in RED in BOLD) + (s" v.$VERSION " in txtClr) + s"$cutie $frameChar" )
    pr(("╚" + frameLine + "╝") in CYAN)
    
    pr("Type a space at the end of your input to enter multiline mode, and a semicolon to exit it." in GREY)
    //pr("Type a comma after your input to enter multiline mode." in GREY)
    
    out.println()
    
    var typingContext = Typing.Ctx.empty
    var printingContext = TypedPrinter.Ctx.init
    
    object BreakOut extends Exception
    
    try while (true) {
      
      reader.setPrompt(first)
      
      var indent = ""
      def readLine = {
        val line = reader.readLine
        if (line != null) indent = line.takeWhile(_ == ' ') //.replaceFirst("  ",pre)
        line
      }
      
      var multilineMode = false
      
      val code = Iterate(readLine) ++ {
        reader.setPrompt(pre)
        Iterate continually {
          
          // does not work well:
          // reader.setPrompt("")
          //out.print(pre)
          //reader.putString(indent)
          //out.flush()
          
          val thr = new Thread() {
            override def run() = {
              //reader.putString(indent)
              //reader.flush()
              reader.getCursorBuffer.write(indent)
              out.print(indent.replace("  ", "· " in GREY))
              reader.flush()
            }
          }
          thr.setPriority(Thread.MAX_PRIORITY)
          thr.setDaemon(true)
          thr.start()
          
          if (multilineMode) readLine else ""
        }
      } flatMap {
        case null => throw BreakOut
        case "" => "" :: Nil
        case str if str.last == ';' => str.init :: "" :: Nil
        case str if str.last == ' ' => multilineMode = true; str :: Nil
        case str => str :: Nil
      } takeWhile (_.nonEmpty) mkString "\n"
      
      def printProblem(typ: Str, org: ?[Origin], msg: Str, fallBackOrgName: ?[Str] = None): Unit = org match {
        case Some(SourceCode(pos)) =>
          out.println(info(s"[$pos] $typ", RED) + (msg in RED))
          out.println(pos.longString)
        case Some(Synthetic(cre, org)) =>
          printProblem(typ, org, msg, cre |> some)
        case Some(MixedOrg(_)) =>
          ??? // TODO
        case None if fallBackOrgName isDefined =>
          out.println(info(s"[synthesized in ${fallBackOrgName get}] $typ", RED) + (msg in RED))
        case None =>
          out.println(info(typ, RED) + (msg in RED))
      }
      
      out.print(post)
      
      if (code replace('\n',' ') forall (_ == ' ')) out.println()
      else try {
        val pgrm = Parser.pgrm(new Parser.lexical.Scanner(code))
        
        pgrm match {
          case Parser.Success(pgrm, _) =>
            try {
              
              out.println(info("Parsed") + ASTPrinter(pgrm).toString)
              
              val septyps = SeparateTypes.vconv.nod(pgrm)
              
              val anf = ToANF.vconv.nod(septyps).toBlock
              //DesugaredPrinter(anf) |> out.println
              out.println(info("Desugared") + DesugaredPrinter(anf))
              
              val (typed, nctx) = typingContext |> Typing.vconv.nod(anf)
              typingContext = nctx
              
              //out.println(info("Typed", GREEN) + TypedPrinter(typed))
              //val typ = Typed.types.Node(typed.md._2, null)
              val typ = typed.md.typ
              //out.println(info("Typed", GREEN) + TypedPrinter.applyTyp(typ))
              val (doc, npctx) = printingContext |> TypedPrinter.tconv.nod(typ)
              printingContext = npctx
              out.println(info("Typed", GREEN) + doc)
              
            } catch {
              case common.CompileError(msg, org) =>
                printProblem("Compile Error", org, msg)
            }
          case Parser.Failure(msg, next) =>
            printProblem("Parse Failure", Some(next.pos |> SourceCode), msg)
            
          case Parser.Error(msg, next) =>
            printProblem("Parse Error", Some(next.pos |> SourceCode), msg)
            
        }
      
      } catch {
        case Parser.lexical.ParseException(msg) =>
          printProblem("Parse Exception", None, msg)
      }
      
    } catch {
      case BreakOut =>
        out.println("Input closed.")
    }
    
  }
  
  
}

