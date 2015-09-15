package parsing

import scala.language.{postfixOps, implicitConversions}
import scala.util.parsing.combinator.syntactical._

import utils._

/**
 * 
 * TODO remove automatic () inference for blocks (probably not useful)
 * 
 * TODO interpret (.a.b x) as OpAppR(...)
 * 
 * FIXME prevent this:
 *   a
 *    + b
 *    c
 *    d
 * 
 * 
 * FIXME:

┌   -- COMMENT
│ a
│ 
└> [2.1] failure: wrong indent
 
┌ (
│   a
│ )
│ 
└> [3.1] failure: block needs to be indented

)
^
 
 
 -- not a problem:
 > map
 |  ls
 |   f
 [3.4] parsed: (map (ls f))
 > a
 |  +b
 |   +c
 [3.5] parsed: ((a +) ((b +) c))
  -- should be written:
 > map
 |     ls
 |   f
 [2.3] parsed: ((map ls) f)
 > a
 |  +b
 |  +c
 [3.4] parsed: ((((a +) b) +) c)
 
 *
 */
object Parser extends TokenParsers {
self =>
  type Tokens = Lexer
  val lexical = new Lexer

  import lexical._
  import AST._

  implicit def keyword(chars: String): Parser[String] =
    accept(Keyword(chars)) ^^ (_.chars)

  def symbol: Parser[String] =
    elem("identifier", _.isInstanceOf[Symbol]) ^^ (_.chars)
  
  case class State(ind: Int, inLambda: Bool, trailingOp: Bool = false) // TODO rm trailingOp
  
  
  def nothing: Parser[Nothing] = acceptIf(_ => false)(_ => "Parser parses nothing") ^^ { _ => wtf }
  
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
  
  def lambda(implicit st: State): Parser[Lambda] = "lambda" !!! (
    (rep1sep(lambdaBranch(st), air("|")) ^^ Lambda.apply)
  | newLine ~> indented(lambdaBlock, strict = st.inLambda)
  )
  
  def lambdaBranch(implicit st: State): Parser[(Term, Term)] = (term(false) <~ air("=>")) ~ genTerm(st) ^^ {case(a~b) => (a,b)}
  
  def atIndent(ind: Int): Parser[Unit] =
    space.? ^? ({ case Some(n) if n == ind =>  case None if ind == 0 => }, _ => "wrong indent")
  
  /** Note: will NOT eat the spaces used for indenting the first line!!! */
  def indented[T](p: Int => Parser[T], strict: Bool = true)(implicit st: State): Parser[T] =
    "indented" !!! (emptyLines ~> guard(space.?) ^? ({ // TODO better error on not defined
      case Some(n) if n > st.ind || (!strict && n == st.ind) => n
      case None if st.ind == 0 && !strict => 0
    }, _ => "block needs to be indented") into p)
  
  implicit class MkTilde[A](a: A) {
    def ~~[B](b: B) = self.~(a, b)
  }
  
  def ReduceOps: (Term ~ List[Operator ~ Option[Term]]) => Term = {
    case t ~ Nil => t
    case t ~ ((op ~ None) :: ls) => ReduceOps(OpAppL(t, op) ~~ ls)
    case t ~ ((op ~ Some(t2)) :: ls) => ReduceOps(App(OpAppL(t, op), t2) ~~ ls)
  }
  
  def compactTerm(implicit st: State): Parser[Term] = "compTerm" !!! (rep1(
    subTerm ~ rep1(operator ~ subTerm.?) ^^ ReduceOps
  | subTerm
  ) ^^ { _ reduceLeft App })
  
  def subTerm(implicit st: State): Parser[Term] = "subTerm" !!! (rep1(
    symbol ^^ Id
  | acceptMatch("string litteral", {case StrLit(str) => Literal(str)})
  | acceptMatch("int litteral", {case IntLit(n) => Literal(n)})
  | "(" ~ ")" ^^^ Unit
  | ("(" ~> air(genTerm(State(st.ind, false))) <~ ")") // TODO ( + newline ...
  | ("(" ~> air(operator) ~ genTerm(State(0, false)).? <~ space.? <~ ")") ^^ {
      case op ~ None => OpTerm(op)
      case op ~ Some(t) => OpAppR(op, t)
    }
  ) ^^ { _ reduceLeft App })
  
  def spaceAppsTerm(implicit st: State): Parser[Term] =
    rep1sep(compactTerm, space) <~ space.? ^^ { _ reduceLeft App }
  
  /** Note: not sure if multiLine param useful */ 
  def term(multiLine: Bool)(implicit st: State): Parser[Term] = "term" !!! ((rep1sep(
    spaceAppsTerm ~ rep(air(operator) ~ spaceAppsTerm.?) ^^ ReduceOps
  // TODO op term here
  , space) <~ space.? ^^ { _ reduceLeft App })
  ~ (if (multiLine) newLine ~> indented(block) else nothing).? ^^ {
    case t ~ None => t
    case t1 ~ Some(t2) => App(t1, t2)
  } | (if (multiLine) newLine ~> indented(block) else nothing)) // TODO prettify/simplify this mess
  
  def genTerm(implicit st: State): Parser[Term] = "genTerm" !!! rep1sep(
    lambda
  | "nl op" !!! ((term(true) <~ newLine) ~ indented(opBlock, strict = false) ^^ {
      case t1 ~ op_ts => op_ts.foldLeft(t1) {
        case(tacc, ops ~ to) => 
          val opst = ops.foldLeft(tacc){case(tacc, op) => OpAppL(tacc, op)}
          to map (App(opst, _)) getOrElse opst
      }
    })
  | term(true)
  , space.?) <~ space.? ^^ { _ reduceLeft App }
  
  def opBlock(n: Int)(implicit st: State) =
    rep1sep((atIndent(n) ~> rep1(air(operator)) <~ space.?) ~ genTerm(State(n+1, false)).?, newLine)
  
  def stmt(implicit st: State): Parser[Stmt] = "stmt" !!! (let | genTerm)
  
  def let(implicit st: State): Parser[Stmt] = "let" !!! ((term(false) <~ air("=")) ~ genTerm) ^^ {
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
      trace(s"┌ $name   on ${in.first} at ${in.pos} offset ${in.offset}")
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
  
//  case object Unit extends Term {
//    def str = s"()"
//  }
  object Unit extends Literal()
  case class Literal[T](value: T) extends Term {
    def str = s"Lit($value)"
  }
  case class Id(s: String) extends Term {
    def str = s"$s"
  }
  case class App(f: Term, a: Term) extends Term {
    def str = s"($f $a)"
  }
  case class OpAppL(t: Term, op: Lexer# Operator) extends Term {
    def str = s"($t ${op.chars})"
  }
  case class OpAppR(op: Lexer# Operator, t: Term, otherOps: List[Lexer# Operator] = Nil) extends Term {
    def str = s"(${op.chars}${otherOps map (_.chars) mkString} $t)"
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
      if (stmts.nonEmpty) s"{${stmts mkString "; "}; $ret}"
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
  

I used to allow the syntax "+b" as in "foo u +b v", where + is any operator and b any term, without any paren, to mean
a right-partial application of + :
    +b == (+ b) == (x => x + b)

However, this made the parser unnecessarily complicated, and it also potentially make the language harder to read, so I
now require parens: (+b)



*/




























