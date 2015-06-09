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
    lexical.delimiters ++= List("=", "=>", "{", "}", "(", ")", ";")

    def termToNode: t.Term => Position => t.Node
    def ptermToNode: PosTerm[t.Term] => t.Node = x => termToNode(x.term)(x.pos)
    def ptermToScoped: PosTerm[t.Term] => t.Scoped = t.Scoped compose ptermToNode

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

    // TODO: The following code is pretty messy, it needs some refactoring and cleaning

    // TODO (long term): remove semicolons in the syntax

    def definition = ident ~ /* (pattern*) ~*/ ("=" ~> (withPos(term))) ^^
      {
        case id ~ expr => t.Let(new t.Symbol(strToId(id)), ptermToScoped(expr))
      }

    def pattern = id // | TODO

    def term: Parser[t.Term] = pattern | lambda

    def lambda: Parser[t.Term] = ("(" ~> withPos(id)) ~ ("=>" ~> withPos(term) <~ ")") ^^
      { case nm ~ e => t.Lambda(t.Extract(ptermToNode(nm)), ptermToScoped(e)) }

    def termOrDef = (definition <~ ";") | (term <~ ";")

    // Block without delimiters
    def internal_block: Parser[t.Block] = (termOrDef*) ^^ (l => t.Block(l))

    def block = "{" ~> internal_block <~ "}"
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
