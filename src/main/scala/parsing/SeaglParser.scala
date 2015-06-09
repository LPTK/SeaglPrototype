package parsing

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator._
import scala.util.parsing.input.Position
import common.Stages._
import common._
import scala.util.parsing.input.Positional
import scala.util.parsing.input.NoPosition

object SeaglParser {
  abstract class ParserTemplate[T <: Ast.TermsTemplate](val t: T) extends StandardTokenParsers {
    lexical.delimiters ++= List("=", "=>", "{", "}", ";")

    def termToNode: t.Term => Position => t.Node
    def ptermToNode: PosTerm[t.Term] => t.Node = x => termToNode(x.term)(x.pos)

    // Great idea: this Positional trait sh*t mixed with a sealed term trait
    // Would be much easier if instead of a Positional token, positioned returned a pair (token, pos)
    // Hence the yoloswag approach to implement withPos
    case class PosTerm[+S](term: S, pos: Position)
    case class PosBox[+S](elt: S) extends Positional

    def withPos[S](p: Parser[S]) = positioned[PosBox[S]](p ^^ { x => PosBox[S](x) }) ^^
      { e => PosTerm(e.elt, e.pos) }

    def strToId: String => t.TermId

    // Always useful
    def fail: Parser[Nothing] = "" ^? PartialFunction.empty[Any, Nothing]

    def keyword = fail

    def id = ident ^^ { l => t.Literal(l) }

    // TODO: remove semicolons
    def definition = ident ~ /* (pattern*) ~*/ ("=" ~> (withPos(expression)) <~ ";") ^^
      {
        case id ~ expr => t.Let(new t.Symbol(strToId(id)), t.Scoped(ptermToNode(expr)),
          termToNode(t.Unit())(NoPosition))
      }

    def pattern = id

    def expression: Parser[t.Term] = id | lambda

    def lambda: Parser[t.Term] = ("{" ~> withPos(id)) ~ ("=>" ~> withPos(expression) <~ "}") ^^
      { case nm ~ e => t.Lambda(t.Extract(ptermToNode(nm)), t.Scoped(ptermToNode(e))) }

    def program = definition
  }

  object TermParser extends ParserTemplate(Ast.values) {
    def termToNode = { x: t.Term => pos: Position => Ast.Node(x, SourceCode(pos)) }

    def strToId = VId.apply
  }

  object TypeParser extends ParserTemplate(Ast.types) {
    def termToNode = { x: t.Term => pos: Position => Ast.Node(x, SourceCode(pos)) }

    def strToId = TId.apply
  }
}
