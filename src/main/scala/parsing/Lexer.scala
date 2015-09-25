package parsing

import java.text.ParseException

import scala.language.implicitConversions
import scala.util.parsing.combinator.lexical._

import utils._

/**
 * TODO space-sensitive grouping
 * 
 * TODO right-associative ops/methods
 *   ie: -.1, 42 :. ls, 42 add.ls
 * 
 * TODO numbers, string lits, tags (identifiers)
 * 
 * 
 */
class Lexer extends Lexical {
  import scala.util.parsing.input.CharArrayReader.EofCh

  case class Symbol(s: String) extends Token {
    def chars = s
  }
  sealed trait Operator extends Token
  case class SymbolOperator(chars: String) extends Operator
  case class MethodOperator(name: String) extends Operator {
    def chars = "." + name
  }
  case class Keyword(s: String) extends Token {
    def chars = s
  }
  case object NewLine extends Token {
    def chars = "[newline]"
  }
  case class Space(size: Int) extends Token {
    def chars = s"[space $size]"
  }
  case class IntLit(n: BigInt) extends Token {
    def chars = n.toString()
  }
  case class StrLit(chars: Str) extends Token
  
  val any = elem("any", _ => true)
  def anyBut(chs: Char*) =
//    elem("any but", ch => !(chs contains ch))
    elem("any char but "+(chs map (_.toInt) mkString ","), !chs.toSet)
  
  val keychars = Set('(', ')', '.', ';', '|', '=', '\\')

  def whitespace: Parser[Null] = Parser(in => Success(null, in))
//  def whitespace: Parser[Any] = rep(('-' ~ '-' | '/' ~ '/') ~ anyBut('\n').* ~ '\n') // doesn't seem to work!?
//  def whitespace: Parser[Any] = rep(('-' ~ '-' | '/' ~ '/') ~ anyBut('\n', EofCh).* ~ (accept(EofCh) | '\n'))
  
  /** Characters in operators */
  def opChar = elem("opchar", ch => !ch.isLetterOrDigit && !(keychars + ' ' + '\n' + '\r')(ch))

  def ident = letter ~ (letter | digit).* ^^ { case l ~ chs => l :: chs mkString ""}
  
//  def error[T](p: Parser[T], msg: Str) = p ~ Parser(in => Error(msg, in)) ^^ (_ => ???)
  def error[T](p: Parser[T], msg: Str) = p ^^ (_ => throw ParseException(msg))
  
  def token: Parser[Token] = (
//      ('-' ~ '-' | '/' ~ '/') ~ anyBut('\n').* ~ '\n' ^^^ Space(0)
      ('-' ~ '-' | '/' ~ '/') ~ anyBut('\n').* ^^^ Space(0)
    | '/' ~ '*' ~ (anyBut('*') | '*' ~ anyBut('/')).* ~ '*' ~ '/' ^^^ Space(1) // TODO nested block comments
    | error('/' ~ '*', "Unclosed block comment")
    | error('\t', "Tabs disallowed")
//    | '/' ~ '*' ^^^ (throw new Exception("Unclosed multiline comment")) // TODO better error
////    | ('-' ~ '-' | '/' ~ '/') ~ anyBut('\n', EofCh).* ~ (accept(EofCh) | '\n') ^^^ Space(1)
//    | '\t' ^^ { _ => throw new Exception("Tabs disallowed") } // TODO
    | rep1(' ') ^^ { ls => Space(ls.size) }
    | ('.' ~> ident) ^^ MethodOperator
    | rep1(digit) ^^ (_ mkString "") ^^ BigInt.apply ^^ IntLit
    | '"' ~> anyBut('"').* <~ '"' ^^ (_.mkString) ^^ StrLit
    
//    | accept("=>") ^^ { c => Keyword(c.toString) }
    | '=' ~ '>' ^^ { c => Keyword("=>") }
    
    | rep1(accept('\n') | '\r') ^^^ NewLine  // Most commonly, I believe, '\n' | '\r' ~ '\n' | '\r'

    | acceptIf(keychars)(ch => s"$ch is not a keyword") ^^ { c => Keyword(c.toString) }
//    | EofCh ^^^ EOF // what's the use of this?!
    | rep1(opChar) ^^ (_ mkString "") ^^ SymbolOperator//^^ { chars => SymbolOperator(chars mkString "") }
    | ident ^^ Symbol
  )
  // Exceptions are not raised here
  //  catch {
  //    case ParseException(msg) => Parser(in => Error(msg, in)) //Parser(_ => ErrorToken(msg))
  //  }
  
  case class ParseException(msg: Str) extends Exception(msg)
  
  override def letter = elem("letter", _.isLetter) // SUPER WEIRD: ScalaJS fails to recognize letters without this!
  
}












