package parsing

import scala.util.parsing.combinator._
import common.Stages._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import common.VId
import common.TId

object SeaglParser {
  abstract class ParserTemplate[T <: Ast.TermsTemplate](val t: T) extends StandardTokenParsers {
    lexical.delimiters ++= List("=", "=>", "{", "}", ";")

    def termToNode: t.Term => t.Node

    def strToId: String => t.Sym

    // Always useful
    def fail: Parser[Nothing] = "" ^? PartialFunction.empty[Any, Nothing]

    def keyword = fail

    def id /*: Parser[t.Literal[String]]*/ = ident ^^ { l => t.Literal(l) }

    // TODO: remove semicolons
    def definition = ident ~ /* (pattern*) ~*/ ("=" ~> expression <~ ";") ^^
      { case id ~ expr => t.Let(strToId(id), termToNode(expr), termToNode(t.Unit())) }

    def pattern = id

    def expression: Parser[t.Term] = id | lambda

    def lambda: Parser[t.Term] = ("{" ~> id) ~ ("=>" ~> expression <~ "}") ^^
      { case nm ~ e => t.Lambda(t.Extract(termToNode(nm)), termToNode(e)) }

    def program = definition
  }

  object TermParser extends ParserTemplate(Ast.values) {
    def termToNode = { x => ??? }

    def strToId = VId.apply
  }

  object TypeParser extends ParserTemplate(Ast.types) {
    def termToNode = { x => ??? }

    def strToId = TId.apply
  }
}
