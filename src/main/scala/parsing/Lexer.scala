package parsing

import scala.language.implicitConversions
import scala.util.parsing.combinator.lexical._

import utils._

/**
 * TODO space-sensitive grouping
 * 
 * TODO right-associative ops/methods
 *   ie: -.1, 42 :. ls, 42 add.ls
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
  
  val any = elem("any", _ => true)
  def anyBut(chs: Char*) =
//    elem("any but", ch => !(chs contains ch))
    elem("any but "+(chs map (_.toInt) mkString ","), !chs.toSet)
  
  val keychars = Set('(', ')', '.', ';', '|', '=')

//  def whitespace: Parser[Null] = Parser(in => Success(null, in))
  def whitespace: Parser[Any] = rep(('-' ~ '-' | '/' ~ '/') ~ anyBut('\n').* ~ '\n')
//  def whitespace: Parser[Any] = rep(('-' ~ '-' | '/' ~ '/') ~ anyBut('\n', EofCh).* ~ (accept(EofCh) | '\n'))
  
  /** Characters in operators */
  def opChar = elem("opchar", ch => !ch.isLetterOrDigit && ch != ' ' && !keychars(ch) && ch != '\n')

  def token: Parser[Token] = (
//      ('-' ~ '-' | '/' ~ '/') ~ anyBut('\n').* ~ '\n' ^^^ Space(0)
      ('.' ~> rep1(letter)) ^^ { chars => MethodOperator(chars mkString "") }
    | rep1(' ') ^^ { ls => Space(ls.size) }
    
//    | accept("=>") ^^ { c => Keyword(c.toString) }
    | '=' ~ '>' ^^ { c => Keyword("=>") }
    
//    | ('-' ~ '-' | '/' ~ '/') ~ anyBut('\n').* ^^^ Space(1)
//    | ('-' ~ '-' | '/' ~ '/') ~ anyBut('\n', EofCh).* ~ (accept(EofCh) | '\n') ^^^ Space(1)
    | '\t' ^^ { _ => throw new Exception("Tabs disallowed") } // TODO
    | '\n' ^^ { _ => NewLine }
    | acceptIf(keychars)(ch => s"$ch is not a keyword") ^^ { c => Keyword(c.toString) }
//    | EofCh ^^^ EOF // what's the use of this?!
    | rep1(opChar) ^^ { chars => SymbolOperator(chars mkString "") }
    | rep1(letter) ^^ { chars => Symbol(chars mkString "") }
  )
}












