package parsing

import scala.language.{postfixOps, implicitConversions}
import scala.util.parsing.combinator.syntactical._

import utils._

/*
Next:
  Var bindings are now considered terms; protect against stupid things
  
  fix comments: currently +-- does not recognize the comment -- wait, that may actually be desirable
  we could force -- to be surrounded by spaces to be valic, allowing an operator --... probably not a good idea, though
*/
/**
 * 
 * TODOne remove automatic () inference for blocks (probably not useful)
 * in fact, just make blocks a list of statements, to be treated later?
 * 
 * TODOmaybenot interpret (.a.b x) as OpAppR(...)
 * 
 * TODOne tuples
 * 
 * TODOne semicolon can have weird behaviors; I could force no multiline for genTerm in (rep1sep(let | genTerm, air(";"))
 * (maybe, prevent trailing semis?)
 * ie:
 *    foo;
 *      bar
 *      baz
 * EDIT: use `;` as a short syntax for let-bindings and impure computations!
 *   x = 1; y = 2; x + y
 *   x = 1; y = 2;
 *     x + y
 *   // x and y not visible here outside the block
 * Allow semis in pattern position: they just allow one to define stuff that can be reused
 *   ls.findFirst{ n = x.getName; x if n == "ok" => n, x }
 * Could be interpreted as an operator...? no, probably confusing
 * Require at least an empty statement to finish a block
 *   a <- foo b
 *   b = bar;  // implicit unit return here only because of `;` not followed by a block
 * 
 * 
 * TODOne operator precedence?
 *     http://jim-mcbeath.blogspot.ch/2008/09/scala-parser-combinators.html#precedencerevisited
 *   Possible?: make |-operators have higher precedence than `;`
 *     foo <| a = 42; a * 2
 *   Question: how to handle this:
 *     
 * 
 * TODO indent-based multiline comments
 *   -- My Doc
 *    goes on these
 *    lines...
 * No, this can be annoying; we may say it only works if followed by nothing - or a \ - or a `-`
 *   --\
 *    My Doc
 *    goes here
 *   ---
 *    My Doc
 *    goes here
 *   --- My Doc
 *    goes on these
 *    lines...
 *   /// My Doc
 *    goes on these
 *    lines...
 * or simply if it starts as the first non-space on a line?
 * 
 * 
 * 
 * TODO op right-asociativity
 Note: scala ref p.85 (http://www.scala-lang.org/docu/files/ScalaReference.pdf)
If there are consecutive infix operations e0 op1 e1 op2 ... opn en with operators op1, ..., opn of the same precedence,
then all these operators must have the same associativity. If all operators are left-associative, the sequence is
interpreted as (... (e0 op1 e1) op2 ...) opn en. Otherwise, if all operators are rightassociative, the sequence is
interpreted as e0 op1 (e1 op2 (...opn en)...).
 * 
 * 
 * FIXME prevent this:
 *   a
 *    + b
 *    c
 *    d
 * 
 * Note: could be useful in some DSLs... not sure if really harmful after all
 * 
 * 
 * FIXME:
 
┌ a >= b
│ 
└> [1.7] parsed: {(a >) = b; ()}
 
 
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

┌   -- a
│ 
└> [1.3] failure: wrong indent
  
 
 
 
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
 
 
┌ a
│ .b c
│  d
│ 
└> [3.3] parsed: ((a .b) (c d))
┌ a
│ +b c
│ +d e
│ 
└> [3.5] parsed: ((((a +) (b c)) +) (d e))
 
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
  
  
  def mk[A,B,R](f: (A, B) => R)(op: A ~ B) = op match { case a ~ b => f(a,b) }
  
  
  def nothing: Parser[Nothing] = acceptIf(_ => false)(_ => "Parser parses nothing") ^^ { _ => wtf }
  
  def space: Parser[Int] = acceptMatch("space", {case Space(n) => n})
  
  def modifier: Parser[Modifier] = acceptMatch("modifier", {case ValueModif => ValueModif  case TypeModif => TypeModif})
  
  def newLine: Parser[Unit] = accept(NewLine) ^^^ ()
  
  def operator: Parser[Operator] = acceptMatch("operator", {
    case op: Operator => op
  })
  def operatorAtLevel(lvl: Int, spaced: Bool = false): Parser[Operator] = operator ^? ({
    case op: MethodOperator if spaced && lvl == precedenceLevels.head => op
    case op: MethodOperator if !spaced && op.precedence == lvl => op
    case op: SymbolOperator if op.precedence == lvl && (spaced || op.sticking) => op
  }, op => s"Wrong precedence for $op")
//  if (reverseForMethods) operator ^? ({
//    case op: MethodOperator if lvl == precedenceLevels.head => op
//    case op: SymbolOperator if op.precedence == lvl => op
//  }, op => s"Wrong precedence for $op") else operator ^? { case op if op.precedence == lvl => op }
  
  def emptyLines: Parser[Unit] = rep(space.? ~ newLine) ^^^ ()
  
  def pgrm = phrase(block(0) <~ emptyLines)

  /** Block of code indented at `ind` */
  def block(ind: Int): Parser[Term] = "block" ! indentedLines(ind, genTerm(State(ind, false))) ^^ Block.apply
  
  def indentedLines[T](ind: Int, p: Parser[T]): Parser[List[T]] = 
    (emptyLines ~> atIndent(ind) ~> p) ~ rep(newLine ~> emptyLines ~> atIndent(ind) ~> p) ^^ mk(_ :: _) /*{
      case x ~ xs => x :: xs
    }*/
  
  def lambdaBlock(ind: Int): Parser[Lambda] =
    indentedLines(ind, "|" ~> space.? ~> lambdaBranch(State(ind, true))) ^^ Lambda.apply
  
  def air[T](p: Parser[T]) = space.? ~> p <~ space.?
  
  def lambda(implicit st: State): Parser[Lambda] = "lambda" ! (
    (rep1sep(lambdaBranch(st), air("|")) ^^ Lambda.apply)
  | newLine ~> indented(lambdaBlock, strict = st.inLambda)
  )
  
  def lambdaBranch(implicit st: State): Parser[(Term, Term)] =
    (term(false) <~ air("=>")) ~ genTerm(st) ^^ mk(_ -> _) //{case(a~b) => (a,b)}
  
  def atIndent(ind: Int): Parser[Unit] =
    space.? ^? ({ case Some(n) if n == ind =>  case None if ind == 0 => }, _ => "wrong indent")
  
  /** Note: will NOT eat the spaces used for indenting the first line!!! */
  def indented[T](p: Int => Parser[T], strict: Bool = true)(implicit st: State): Parser[T] =
    "indented" ! (emptyLines ~> guard(space.?) ^? ({ // TODO better error on not defined
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
  
  def opAppLefts(subParser: Parser[Term], precLevel: Int = precedenceLevels.head)(implicit st: State): Parser[Term] = s"binary($precLevel)" ! (
    if (precLevel > precedenceLevels.last) subParser
    else opAppLefts(subParser, precLevel+1) ~ rep(
      operatorAtLevel(precLevel) ~ opAppLefts(subParser, precLevel+1).?
    ) ^^ ReduceOps
  )
  
  def compactTerm(implicit st: State): Parser[Term] = "compTerm" !! (rep1(
//    subTerm ~ rep1(operator ~ subTerm.?) ^^ ReduceOps
    opAppLefts(subTerm)
  | subTerm
  ) ^^ { _ reduceLeft App })
  
  def subTerm(implicit st: State): Parser[Term] = "subTerm" ! (rep1(
    symbol ^^ Id
  | acceptMatch("string litteral", {case StrLit(str) => Literal(str)})
  | acceptMatch("int litteral", {case IntLit(n) => Literal(n)})
  | "(" ~ ")" ^^^ Unit
  | ("(" ~> air(genTerm(State(st.ind, false))) <~ ")") // TODOnot ( + newline ... <- actually it's nice NOT to support it
  | ("(" ~> air(operator) ~ genTerm(State(0, false)).? <~ space.? <~ ")") ^^ {
      case op ~ None => OpTerm(op)
      case op ~ Some(t) => OpAppR(op, t)
    }
  ) ^^ { _ reduceLeft App })
  
  def spaceAppsTerm(implicit st: State): Parser[Term] =
//    rep1sep(compactTerm, space) <~ space.? ^^ { _ reduceLeft App }
    rep1sep(compactTerm, space) ^^ { _ reduceLeft App }
  
  def spacedOpAppLefts(precLevel: Int = precedenceLevels.head)(implicit st: State): Parser[Term] = s"spBinary($precLevel)" ! (
    if (precLevel > precedenceLevels.last) spaceAppsTerm
    else spacedOpAppLefts(precLevel+1) ~ rep(air(operatorAtLevel(precLevel, true)) ~ spacedOpAppLefts(precLevel+1).?) ^^ ReduceOps
  )
  
  /** Note: not sure if multiLine param useful */ 
  def term(multiLine: Bool)(implicit st: State): Parser[Term] = "term" ! (
    (spacedOpAppLefts() <~ space.?)
  ~ (if (multiLine) newLine ~> indented(block) else nothing).? ^^ {
    case t ~ None => t
    case t1 ~ Some(t2) => App(t1, t2)
  })
  
  def genTerm(implicit st: State, trailingOp: Bool = false): Parser[Term] =
//    rep1sep(genTermWithoutSemi, air(";")) ^^ Block.apply
      rep(genTermWithoutSemi(false) <~ air(";")) ~ genTermWithoutSemi(true) ^^ mk((ls,t) => Block(ls :+ t)) //{ case ls ~ t => Block(ls :+ t) }
  
  /** genTerm adds to term: lambdas, newline ops, newline blocks */
  def genTermWithoutSemi(multiLine: Bool)(implicit st: State, trailingOp: Bool = false): Parser[Term] = "genTerm" !! (rep1sep(
    lambda
  | "nl op" ! (if (multiLine) (
        (term(true) <~ newLine) ~ indented(opBlock, strict = false)
      ) ^^ {
      case t1 ~ op_ts => op_ts.foldLeft(t1) {
        case(tacc, ops ~ to) => 
          val opst = ops.foldLeft(tacc){case(tacc, op) => OpAppL(tacc, op)}
          to map (App(opst, _)) getOrElse opst
      }
    } else nothing)
  | let
  | term(multiLine)
  | newLine ~> indented(block, !trailingOp)
  , space.?) <~ space.? ^^ { _ reduceLeft App })
  
  def opBlock(n: Int)(implicit st: State) = "opBlock" !! 
    rep1sep((atIndent(n) ~> rep1(air(operator)) <~ space.?) ~ genTerm(State(n+1, false), true).?, newLine)
  
//  def stmt(implicit st: State): Parser[Stmt] = "stmt" ! (rep1sep(let | genTerm, air(";")) ^^ {
//    case stmt :: Nil => stmt
//    case ls => Block(ls)
//  })
  
  def binding(implicit st: State): Parser[Term ~ Term] = (spacedOpAppLefts() <~ air("=")) ~ genTermWithoutSemi(true)
  
//  def let(implicit st: State): Parser[Term] = "let" ! ((termWithoutSemis(false) <~ air("=")) ~ genTerm) ^^ {
  def let(implicit st: State): Parser[Term] = "let" ! (
    (modifier <~ space.?).? ~ binding ^^ { 
      case mo ~ (a ~ b) => Let(a, b, mo) }
  | (modifier <~ space.? ~ newLine) ~ indented(ind => indentedLines(ind, binding(st copy (ind = ind)))) ^^ {
      case m ~ ls => Block(ls map { case a ~ b => Let(a, b, Some(m)) }) }
  )
  
  
  val init = State(0, false)
//  def repl = phrase(emptyLines ~> (stmt(init) | block(0)) <~ emptyLines)
  
  
  
  
  
  
  
  
  
  
  
  
  
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
//    def !![T](parser: Parser[T]) = new Wrap(name, parser)
    def !![T](parser: Parser[T]) = parser
    def ![T](parser: Parser[T]) = parser
  }
  
}








object AST {

//  sealed trait Stmt {
//    def str: Str
//    
//    override def toString = str
//  }

  case class Let(pattern: Term, value: Term, modif: Option[Parser.lexical.Modifier] = None) extends Term {//Stmt {
    def str = (modif map (_.toString + " ") getOrElse "") + s"$pattern = $value"
  }

  
  sealed trait Term { //extends Stmt
    def str: Str
    override def toString = str
  }
  
//  case object Unit extends Term {
//    def str = s"()"
//  }
  object Unit extends Literal()
  case class Literal[T](value: T) extends Term {
    def str = value match {
      case () => "()"
      case _ => s"Lit($value)"
    }
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
  case class Block(stmts: List[Term], ret: Term) extends Term {
    def str =
      if (stmts.nonEmpty) s"{${stmts mkString "; "}; $ret}"
      else ret.toString
  }
  object Block { def apply(lines: List[Term]): Term = mkBlock(lines: _*) }
//  object Block { val apply = mkBlock _ }
  def mkBlock(lines: Term*): Term = lines match {
    case Seq(t: Term) => t
    case init :+ (t: Term) => Block(init.toList, t)
    case ls => Block(ls.toList, Unit)
  }

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






:: Alternative Block Syntax

Indented blocks will be interpreted as multiple parameters instead of an imperative block
Imp blocks could be done with an (possibly override-able) `do` keyword

  foo
    bar
    baz
  ==
  foo bar baz

  foo do
    bar
    baz


Alternatively, use `;`, but that's a pity for a space-based syntax...
It means we don't even actually need the indentations anymore

  foo
    bar;
    baz

  if x < y
    r = sqrt (y - x);
    print r;
    r
    print "nope";
    0

And how to express a multiline statement? Where does the semi-colon go? (it's ambiguous)

Best is probably to use `do`


Or: let statements need no semi; only impure calls do...
  
  if x < y
    r = sqrt (y - x)
    print r;
    r
    print "nope";
    0

No, that's not clear at all


Note: in the old way, it is possible to obtain nice call argument chaining by calling `.app` explicitly:

  foo
  .app bar
  .app baz

  map
  .app ls
  .app f
or
  map
    ls
  .app f



TODO: implicit operator repetition

A trailing `\[op]` introduces a list of indented blocks interpreted as a list of terms joined by [op].
We define `[op]\` as `[op] \[op]`

Examples:

  foo = \+
    bar
    baz
    bal
    
  // ie
  
  foo =
    bar
    + baz
    + bal
  
  
  map .app\
    ls
    f

Most importantly, comma `,` is defined as a normal operator, and can consequently be used that way
Note: This makes it important to find a good replacement for '=' in named argument binding, as otherwise it will be very
confusing, in the presence of IOR

  graph.render <| 0, 0, "My Graph", autoSized := true, color := `Green
  
  graph.render <| 0, 0,\
    "My Graph"
    autoSized := true
    color := `Green
  
  
Problem: using the previous rules for op spacing, `a[op] b[op] c` will resolve to `(a[op]) (b[op]) c`
It's important for method ops, in things like `a.foo b.x c`
..could use a rule for non-method ops to not bind this way


TODO: disallow multiline parens and use paren block continuation instead
Syntax: \)
Can be combined with implicit operator repetition

  print (a, b, \)
    foo,
     bar
  
  print (a, b,\)
    foo
    bar
  
  
Note:
  print (\,)
    a
    b
    foo
    bar
is useless... just use:
  print \,
    a
    b
    foo
    bar




:: A way to view commas as an operator

Main requirement is to remove the stuck-to rule for some ops: `a, b, c` == `(a, b), c` and not `(a,) (b,) c`

Otherwise, we'd need complex type hacking (redefining .apply for the result of (x,))
The basic idea would be:

  (,) x = `Comma x
  app (`Comma x) =
  | `Comma y => `Comma (Pair x y)
  | z => Pair x z
  
But note that we still have:
  f a, b, c  ==  (f (a ,)) (b ,) c

... which makes the whole idea collapse (it would be a very surprising/unintuitive behavior)



:: MethodOps precedence

They are the _most_ binding when used without space, and the _least_ binding when used with a space!!
For example:
  1 + a.f   ==  1 + (a .f)
  a .f + 1  ==  (1 + a) .f 

That allows to conveniently write inline ITE:
  x > y ? foo .else bar
ie
  ((x > y) ? foo) .else bar

*/




























