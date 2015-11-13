package main

import java.io.PrintWriter

import front2.DesugaredPrinter
import front2.SeparateTypes
import front2.ToANF
import utils._  
import common._

import jline._
import console._
import parsing.Parser
import front2.ASTPrinter

import scala.util.parsing.input.Position

import completer._

/**
  * TODO: handle empty indented lines at end of input... ?
  * eg:
  *   """
  *   foo
  *     bar
  *     
  *   
  *   """
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
              reader.putString(indent)
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
      
      def printParsePb(typ: Str, pos: ?[Position], msg: Str) = {
        out.println(info((pos map (p => s"[$p] ") getOrElse "") + s"Parse $typ", RED) + (msg in RED))
        pos foreach (p => out.println(p.longString))
      }
      
      out.print(post)
      
      if (code replace('\n',' ') forall (_ == ' ')) out.println
      else try {
        val pgrm = Parser.pgrm(new Parser.lexical.Scanner(code))
        
        pgrm match {
          case Parser.Success(pgrm, _) =>
            try {
              out.println(info("Parsed") + ASTPrinter(pgrm).toString)
              val septyps = SeparateTypes.vconv.nod(pgrm)    
              val anf = ToANF.vconv.nod(septyps).toBlock
              DesugaredPrinter(anf) |> out.println
            } catch {
              case common.CompileError(msg) => "Compile Error: "+msg
            }
          case Parser.Failure(msg, next) =>
            printParsePb("Failure", Some(next.pos), msg)
            
          case Parser.Error(msg, next) =>
            printParsePb("Error", Some(next.pos), msg)
            
        }
      
      } catch {
        case Parser.lexical.ParseException(msg) =>
          printParsePb("Exception", None, msg)
      }
      
    } catch {
      case BreakOut =>
        println("Input closed.")
    }
    
  }
  
  
}

