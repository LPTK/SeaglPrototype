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
    
    object BreakOut extends Exception
    
    try while (true) {
      
      reader.setPrompt(first)
      
      var indent = ""
      def readLine = {
        val line = reader.readLine
        if (line != null) indent = line.takeWhile(_ == ' ') //.replaceFirst("  ",pre)
        line
      }
      
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
          
          readLine
        }
      } flatMap {
        case null => throw BreakOut
        case "" => "" :: Nil
        case str if str.last == ';' => str.init :: "" :: Nil
        case str => str :: Nil
      } takeWhile (_.nonEmpty) mkString "\n"
      
      def printProblem(typ: Str, pos: ?[Position], msg: Str) = {
        out.println(info((pos map (p => s"[$p] ") getOrElse "") + s"$typ", RED) + (msg in RED))
        pos foreach (p => out.println(p.longString))
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
              
              val typingContext = Typing.Ctx.empty    
              
              val (typed, nctx) = typingContext |> Typing.vconv.nod(anf)   
              //out.println(info("Typed", GREEN) + TypedPrinter(typed))
              val typ = Typed.types.Node(typed.md._2, null)
              out.println(info("Typed", GREEN) + TypedPrinter.applyTyp(typ))
              
            } catch {
              case common.CompileError(msg) =>
                //out.println("Compile Error: "+msg)
                printProblem("Compile Error", Some(pgrm.pos), msg) // TODO better position
              //case e => out.println(e.toString)
            }
          case Parser.Failure(msg, next) =>
            printProblem("Parse Failure", Some(next.pos), msg)
            
          case Parser.Error(msg, next) =>
            printProblem("Parse Error", Some(next.pos), msg)
            
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

