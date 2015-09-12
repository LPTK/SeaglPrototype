package parsing

import scala.language.implicitConversions
import scala.util.parsing.combinator.syntactical._

import utils._

/**
 * FIXME prevent this:
 *   a
 *    + b
 *    c
 *    d
 * 
 * FIXME:
> a + b + c
[1.10] parsed: (((a +) (b +)) c)
> a+b+c
[1.6] parsed: (((a +) (b +)) c)
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

  /** Block of code indented at `ind` */
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
  
  def lambdaBranch(st: State): Parser[(Term, Term)] = (term(st, false) <~ air("=>")) ~ genTerm(st) ^^ {
    case a ~ b => (a,b)
  }
  
  def atIndent(ind: Int): Parser[Unit] =
    space.? ^? ({ case Some(n) if n == ind =>  case None if ind == 0 => }, _ => "wrong indent")
  
  def indentedBlock(st: State): Parser[Term] = "indentedBlock" !!! indented(st, block)
  
  /** Note: will NOT eat the spaces used for indenting the first line!!! */
  def indented[T](st: State, p: Int => Parser[T], strict: Bool = true): Parser[T] =
    "indented" !!! (emptyLines ~> guard(space.?) ^? ({ // TODO better error on not defined
      case Some(n) if n > st.ind || (!strict && n == st.ind) => n
      case None if st.ind == 0 && !strict => 0
    }, _ => "block needs to be indented") into p)
  
//  def parens()
  
  def compactTerm(st: State, multiLine: Bool): Parser[Term] = rep1(
    subTerm(st, multiLine) ~ rep1(operator) ^^ { case t ~ ops => ops.foldLeft(t)(OpApp) }
  | operator ~ compactTerm(st, multiLine) ^^ { case op ~ t => App(OpTerm(op), t) } // FIXME wrong
  | subTerm(st, multiLine)
  ) ^^ { _ reduceLeft App }
  
  def subTerm(st: State, multiLine: Bool): Parser[Term] = rep1(
    symbol ^^ Id
  | ("(" ~> space.? ~> genTerm(State(0, false)) <~ space.? <~ ")") // TODO ( + newline ... TODO not 0
  | ("(" ~> space.? ~> operator <~ space.? <~ ")") ^^ OpTerm
//  | newLine ~> indented(st, n => (operator <~ space.?) ~ genTerm(State(n, false, true)) ^^ { case op ~ t => ??? })
//  | (if (multiLine) newLine ~> indentedBlock(st) else acceptIf(_ => false)(_ => ""))
  | (if (multiLine) newLine ~> indentedBlock(st) else symbol ^^ Id /* <- just a dummy one that will never parse */)
  ) ^^ { _ reduceLeft App }
  
  def term(st: State, multiLine: Bool): Parser[Term] = "term" !!! (rep1sep(
      compactTerm(st, multiLine) ~ rep1(space ~> operator) ^^ { case t ~ ops => ops.foldLeft(t)(OpApp) }
    | compactTerm(st, multiLine)
  , space) <~ space.? ^^ { _ reduceLeft App })
  
  def genTerm(st: State): Parser[Term] = rep1sep(
    lambda(st)
//  | "nl op" !!! {(term(st, false) <~ newLine) ~ indented(st, n => air(operator) ~ genTerm(State(n, false, true)))} ^^ {
//      case t1 ~ (op ~ t2) => App(OpApp(t1, op), t2)
//    }
  | "nl op" !!! {(term(st, false) <~ newLine) ~ indented(st, n =>
      rep1sep((atIndent(n) ~> operator <~ space.?) ~ genTerm(State(n, false, true)) /*^^ {case a~b=>(a,b)}*/, newLine)
    )} ^^ {
//      case t1 ~ (op ~ t2) => App(OpApp(t1, op), t2)
      case t1 ~ op_ts => op_ts.foldLeft(t1){ case(tacc, op ~ t) => App(OpApp(tacc, op), t) }
    }
  | term(st, true)
  , space.?) <~ space.? ^^ { _ reduceLeft App }
  
  def stmt(st: State): Parser[Stmt] = "stmt" !!! (let(st) | genTerm(st))
  
  def let(st: State): Parser[Stmt] = (term(st, false) <~ air("=")) ~ genTerm(st) ^^ {
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
  
  var ind = 0
  def trace(x: Any) = println("│ " * ind + x)
  class Wrap[+T](name: String, parser: Parser[T]) extends Parser[T] {
//    def apply(in: Input): ParseResult[T] = {
//      val first = in.first
//      val pos = in.pos
//      val offset = in.offset
//      val t = parser.apply(in)
//      println(name + " for token " + first +
//        " at position " + pos + " offset " + offset + " returns " + t)
//      t
//    }
    def apply(in: Input): ParseResult[T] = {
      trace(s"┌ trying $name on ${in.first} at ${in.pos} offset ${in.offset}")
      ind += 1
      val t = parser.apply(in)
      ind -= 1
      trace("└─> " + (t match { // →
        case Success(v, _) => v
        case NoSuccess(msg, _) => "Fail: " + msg
      }))
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




























