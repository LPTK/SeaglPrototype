package parsing

import scala.language.implicitConversions
import scala.util.parsing.combinator.syntactical._

import utils._

/**
 * TODO newlined ops
 *    foo
 *      + bar
 *      + baz
 * 
 * TODO trailing ops
 *    foo +
 *      bar +
 *        baz
 * 
 */
object Parser extends TokenParsers {
  type Tokens = Lexer
  val lexical = new Lexer

  import lexical._
  import AST._

  implicit def keyword(chars: String): Parser[String] =
    accept(Keyword(chars)) ^^ (_.chars)

  def symbol: Parser[String] =
    elem("identifier", _.isInstanceOf[Symbol]) ^^ (_.chars)
  
  case class State(ind: Int, inLambda: Bool, trailingOp: Bool = false)
  
  def space: Parser[Int] = acceptMatch("space", {case Space(n) => n})
  
  def newLine: Parser[Unit] = accept(NewLine) ^^^ ()
  
  def operator: Parser[Operator] = acceptMatch("operator", {
    case op: Operator => op
  })
  
  def emptyLines: Parser[Unit] = rep(space.? ~ newLine) ^^^ ()
  
  def pgrm = phrase(block(0) <~ emptyLines)

  /** Block of code indented at `ind`  */
  def block(ind: Int): Parser[Term] = "block" !!! {
    val st = State(ind, false)
    (emptyLines ~> atIndent(ind) ~> stmt(st)) ~ rep(newLine ~> emptyLines ~> atIndent(ind) ~> stmt(st)) ^^ {
      case stmt ~ stmts => Block(stmt :: stmts)
    }
  }
  def lambdaBlock(ind: Int): Parser[Lambda] =
    (emptyLines ~> atIndent(ind) ~> "|" ~> space.? ~> lambdaBranch(State(ind, true))) ~ rep(newLine ~>
     emptyLines ~> atIndent(ind) ~> "|" ~> space.? ~> lambdaBranch(State(ind, true))) ^^ {
      case br ~ brs => Lambda(br :: brs)
    }
  
  def air[T](p: Parser[T]) = space.? ~> p <~ space.?
  
  def lambda(st: State): Parser[Lambda] = (
    (/*air("|").? ~>*/ rep1sep(lambdaBranch(st), air("|")) ^^ Lambda.apply)
  | (newLine /*| guard("|")*/) ~> indented(st, lambdaBlock, st.inLambda)
  )
  
  def lambdaBranch(st: State): Parser[(Term, Term)] = (term(st) <~ air("=>")) ~ genTerm(st) ^^ {
    case a ~ b => (a,b)
  }
  
  def atIndent(ind: Int): Parser[Unit] =
    space.? ^? ({ case Some(n) if n == ind =>  case None if ind == 0 => }, _ => "wrong indent")
  
  def indentedBlock(st: State): Parser[Term] = "indentedBlock" !!! indented(st, block)
  
  def indented[T](st: State, p: Int => Parser[T], strict: Bool = true): Parser[T] =
    "indentedBlock" !!! (emptyLines ~> guard(space.?) ^? ({ // TODO better error on not defined
      case Some(n) if n > st.ind || (!strict && n == st.ind) => n
      case None if st.ind == 0 && !strict => 0
    }, _ => "block needs to be indented") into p)
  
//  def parens()
  
  def compactTerm(st: State): Parser[Term] = rep1(
    subTerm(st) ~ rep1(operator) ^^ { case t ~ ops => ops.foldLeft(t)(OpApp) }
  | operator ~ compactTerm(st) ^^ { case op ~ t => App(OpTerm(op), t) } // FIXME wrong
  | subTerm(st)
  ) ^^ { _ reduceLeft App }
  
  def subTerm(st: State): Parser[Term] = rep1(
    symbol ^^ Id
  | ("(" ~> space.? ~> genTerm(State(0, false)) <~ space.? <~ ")") // TODO ( + newline ... TODO not 0
  | ("(" ~> space.? ~> operator <~ space.? <~ ")") ^^ OpTerm
//  | newLine ~> indented(st, n => (operator <~ space.?) ~ genTerm(State(n, false, true)) ^^ { case op ~ t => ??? })
  | newLine ~> indentedBlock(st)
  ) ^^ { _ reduceLeft App }
  
  def term(st: State): Parser[Term] = "term" !!! (rep1sep(
      compactTerm(st) ~ rep1(space ~> operator) ^^ { case t ~ ops => ops.foldLeft(t)(OpApp) }
    | compactTerm(st)
  , space) <~ space.? ^^ { _ reduceLeft App })
  
  def genTerm(st: State): Parser[Term] = rep1sep(
    lambda(st)
//  | "nl op" !!! {(term(st) <~ newLine) ~ indented(st, n => (operator <~ space.?) ~ genTerm(State(n, false, true)))} ^^ {
//      case t1 ~ (op ~ t2) => App(OpApp(t1, op), t2)
//    }
  | "nl op" !!! {(term(st) <~ newLine) ~ indented(st, n => (operator <~ space.?) ~ genTerm(State(n, false, true)))} ^^ {
      case t1 ~ (op ~ t2) => App(OpApp(t1, op), t2)
    }
  | term(st)
  , space.?) <~ space.? ^^ { _ reduceLeft App }
  
  def stmt(st: State): Parser[Stmt] = "stmt" !!! (let(st) | genTerm(st))
  
  def let(st: State): Parser[Stmt] = (term(st) <~ air("=")) ~ genTerm(st) ^^ {
    case a ~ b => Let(a, b)
  }
  
  
  val init = State(0, false)
  def repl = phrase(emptyLines ~> (stmt(init) | block(0)) <~ emptyLines)
  
  
  
  def parse[T](str: String, p: Parser[T] = pgrm): ParseResult[T] = {
    
    def prrrrt(sc: lexical.Scanner) =
      (Iterator iterate sc)(_.rest) takeWhile (!_.atEnd) map (_.first) foreach debug
    
    val sc = new lexical.Scanner(str)
    prrrrt(sc)
    debug(" ---")
    val r = phrase(p)(sc)
    debug(" ---")
    r
  }
//  def debug(x: Any) = println(x)
  def debug(x: Any) {}
  
  
  class Wrap[+T](name: String, parser: Parser[T]) extends Parser[T] {
    def apply(in: Input): ParseResult[T] = {
      val first = in.first
      val pos = in.pos
      val offset = in.offset
      val t = parser.apply(in)
      println(name + " for token " + first +
        " at position " + pos + " offset " + offset + " returns " + t)
      t
    }
  }

  implicit class Wrapper(val name: String) {
//    def !!![T](parser: Parser[T]) = new Wrap(name, parser)
    def !!![T](parser: Parser[T]) = parser
  }
  
}

object AST {

  sealed trait Stmt {
    def str: Str
    
    override def toString = str
  }

  case class Let(pattern: Term, value: Term) extends Stmt {
    def str = s"$pattern = $value"
  }

  
  sealed trait Term extends Stmt
  
  case object Unit extends Term {
    def str = s"()"
  }
  case class Id(s: String) extends Term {
    def str = s"$s"
  }
  case class App(f: Term, a: Term) extends Term {
    def str = s"($f $a)"
  }
  case class OpApp(t: Term, op: Lexer# Operator) extends Term {
    def str = s"($t ${op.chars})"
  }
  case class OpTerm(op: Lexer# Operator) extends Term {
    def str = s"(${op.chars})"
  }
  case class Lambda(branches: List[(Term, Term)]) extends Term {
    def str = "{" + (branches map {case(a,b) => s"$a => $b"} mkString " | ") + "}"
  }
  object Lambda {
    def apply(branches: (Term, Term)*): Lambda = Lambda(branches.toList)
  }
  case class Block(stmts: List[Stmt], ret: Term) extends Term {
    def str =
      if (stmts.size > 0) s"{${stmts mkString ";"}; $ret}"
      else ret.toString
  }
  object Block { def apply(lines: List[Stmt]): Term = lines match {
    case (t: Term) :: Nil => t
    case init :+ (t: Term) => Block(init, t)
    case ls => Block(ls, Unit)
  }}

}



/*

Interaction between App and OpApp

  foo a b + c d
  foo a b.bar c d

  a.foo.foo(b) c
  a.foo.foo(b).bar
  
  





*/




























