package parsing

import org.scalatest.{ FlatSpec, ShouldMatchers }
import parsing.Parser.lexical.{ SymbolOperator, ValueModif, TypeModif }

import utils._
import Parser._
import AST._

/**
 * TODO test for empty lines
 * 
 */
class ParserTests extends FlatSpec with ShouldMatchers {
  
  implicit def sym2str(s: Sym): Str = s match { case Sym(str) => str }
  implicit def sym2id(s: Sym): Id = s match { case Sym(str) => Id(str) }
  
//  val plus = Parser.lexical.SymbolOperator("+")
//  val minus = Parser.lexical.SymbolOperator("-")
  val List(plus, minus, times, gt, qmark, comma) =
    List("+", "-", "*", ">", "?", ",") map Parser.lexical.SymbolOperator
  
  val List(foo, bar, map, els) = List("foo", "bar", "map", "else") map Parser.lexical.MethodOperator
  
  def bin(lhs: Term, op: Parser.lexical.Operator, rhs: Term) = App(OpAppL(lhs, op), rhs)
  
  
  "parsing id" should "work" in {
    // Note: Starting with an indent is wrong, eg: " a"
    
//    tests("a" -> Id('a), "a " -> Id('a))
    tests(
      Seq("a", "a ", "a  ", "a--\n", "a -- ,.;'[(\n") -> 'a.id
    )
    
  }
  
  "parsing litterals" should "work" in {
    tests(
      "()" -> Unit,
      "42" -> Literal(42),
      "\"ok\"" -> Literal("ok")
    )
  }
  
  "parsing app" should "work" in tests(
  
    Seq("a b", "a(b)", "(a)b", "(a)(b)", "(a) (b)") -> App('a, 'b),
  
    Seq("a b c", "(a b)c") -> App(App('a, 'b), 'c),
  
    Seq("a (b c)", "a (b)(c)") -> App('a, App('b, 'c)),
  
    "a b c d" -> App(App(App('a, 'b), 'c), 'd),
    "a (b c) d" -> App(App('a, App('b, 'c)), 'd),
  
    "a b" -> App('a, 'b)
  )
  
  "parsing equals-operators" should "work" in tests(
    "a == b" -> bin('a, SymbolOperator("=="), 'b),
    "a === b" -> bin('a, SymbolOperator("==="), 'b),
    "a =/= b" -> bin('a, SymbolOperator("=/="), 'b),
    "a /= b" -> bin('a, SymbolOperator("/="), 'b),
    "a =/ b" -> bin('a, SymbolOperator("=/"), 'b),
    "a /=/ b" -> bin('a, SymbolOperator("/=/"), 'b)
  )
  
  "parsing partial operator app" should "work" in tests(
    
    Seq("a +", "a+", "(a+)") -> OpAppL('a, plus),
    Seq("a b+", "a (b+)", "a(b +)") -> App('a, OpAppL('b, plus)),
    Seq("a b+ c", "a (b+) c", "a(b +)c") -> App(App('a, OpAppL('b, plus)), 'c),
  
//    Seq("a .foo +", "a.foo +", "a.foo+") -> OpAppL(OpAppL('a, foo), plus), // Wrong precedence
    Seq("a .foo .bar", "a.foo .bar", "a.foo.bar") -> OpAppL(OpAppL('a, foo), bar),
  
    Seq("(.foo)"/*, "(\n .foo \n)" FIXME*/) -> OpTerm(foo),
  
    Seq("(.foo x)", "(.foo\n x)") -> OpAppR(foo, 'x)
    
  )
  "precedence" should "work" in tests(
    // Precedence with operators
  
    Seq("a b .foo c d", "(a b).foo(c d)") -> App(OpAppL(App('a,'b), foo), App('c, 'd)),
    
    // Because of the stick-rule, `a.foo` is bound more stringly than `a .foo` in `a .foo b`, which means the operator
    // loses its separation function.
    // It's probably the right thing to do since one would expect `ls.fold z f` to parse like `(ls.fold) z f`.
    Seq("(a .foo c) d", "(a.foo) c d", "a.foo c d") -> App(bin('a, foo, 'c), 'd),
    Seq("(a b) .foo c", "a b .foo c", "(a b).foo c") -> bin(App('a,'b), foo, 'c),
    
    Seq("a+b+c", "a+b+ c", "a+b + c", "a+ b + c", "a +b +c", "a + b + c") -> App(OpAppL(App(OpAppL('a, plus), 'b), plus), 'c),
    Seq("a*b+c", "a*b+ c", "a*b + c", "a* b + c", "a *b +c", "a * b + c") -> App(OpAppL(App(OpAppL('a, times), 'b), plus), 'c),
    Seq("a+b*c", "a+ b*c", "a + b*c", "a + b* c", "a +b *c", "a + b * c") -> App(OpAppL('a, plus), App(OpAppL('b, times), 'c)),
    Seq("a+b.foo*c", "a + b.foo* c", "a + b.foo * c", "a + (b .foo) * c") -> App(OpAppL('a, plus), App(OpAppL(OpAppL('b,foo), times), 'c)),
    
    // methods become the least binding when used with spaces
    Seq("1 + a.foo", "1 + (a .foo)") -> bin(Literal(1), plus, OpAppL('a, foo)),
    Seq("1 + a .foo", "(1 + a).foo") -> OpAppL(bin(Literal(1), plus, 'a), foo),
    Seq("a+b .foo b*c", "(a+b).foo b*c", "(a+b).foo(b*c)", "a+b .foo b * c", "a + b .foo b * c") ->
      bin(bin('a,plus,'b), foo, bin('b, times, 'c)),
    Seq("a+b.foo*c") -> App(OpAppL('a, plus), App(OpAppL(OpAppL('b,foo), times), 'c)),
  
    // comma is a "non-sticking" operator
    Seq("a,b,c", "a,b, c", "a, b, c", "a , b , c", "a ,b ,c") -> bin(bin('a, comma, 'b), comma, 'c),
    Seq(/*"f a,b,c"/*FIXME?*/,*/ "f (a, b, c)") -> App('f, bin(bin('a, comma, 'b), comma, 'c)),
    Seq("f a, g b, h c", "(f a),(g b),(h c)") -> bin(bin(App('f, 'a), comma, App('g, 'b)), comma, App('h, 'c)),
  
    // some SymOps precedence rules
    "a |> b * c" -> bin('a, SymbolOperator("|>"), bin('b, times, 'c)),
    "a <| b * c" -> bin('a, SymbolOperator("<|"), bin('b, times, 'c)),
    "a |* b + c" -> bin('a, SymbolOperator("|*"), bin('b, plus, 'c)),
    "a *| b + c" -> bin('a, SymbolOperator("*|"), bin('b, plus, 'c))
    
  )
  
  val apb_pc = App(OpAppL(App(OpAppL('a, plus), 'b), plus), 'c)
  val ap_bpc = App(OpAppL('a, plus), App(OpAppL('b, plus), 'c))
  
  "parsing operator app" should "work" in tests(
    
    Seq("a + b", "a+ b", "a+b", "a\n +b", "a\n + b") -> App(OpAppL('a, plus), 'b),
    Seq("a .foo b", "a.foo b", "a.foo(b)", "(a.foo) b", "(a .foo)b") -> App(OpAppL('a, foo), 'b),
  
    Seq("a+b+c", "a+b + c", "a + b + c") -> apb_pc
    
  )
  
  "parsing newline operators" should "work" in tests(
  
    Seq("a + b + c", "a\n + b\n + c", "a\n +b\n +c", "a \n +b \n +c", "a\n+ b\n+ c") -> apb_pc,
  
    Seq("a + b+c", "a\n + b\n  + c") -> ap_bpc,
    
    Seq("(a.foo d).bar", "a.foo d .bar", "a .foo d .bar",
"""
a
.foo
 d
.bar""",
"""
a
.foo
  d
.bar""") -> OpAppL(App(OpAppL('a, foo), 'd), bar),
      
    Seq("a.foo (c d) .bar", //FIXME "a.foo c d .bar",
"""
a
.foo c
 d
.bar
""") -> OpAppL(App(OpAppL('a, foo), App('c, 'd)), bar),
  
    Seq("(a.foo c) d .bar",
"""a.foo c
 d
.bar
""") -> OpAppL(App(App(OpAppL('a, foo),'c), 'd), bar)
  
  )
  
  "parsing blocks" should "work" in tests(
  
    Seq("a\n  x = b\n  x a", "a(\n x = b\n x a)") -> App('a, Block(Let('x, 'b)::Nil, App('x, 'a))),
    Seq("a\nx = b\nx a", "a;x=b;x a", "a; x=b; x a", "(a; x = b; x a)") -> Block('a::Let('x, 'b)::Nil, App('x, 'a)),
    Seq("a;\n x=b\n x a") -> Block('a::Block(Let('x, 'b)::Nil, App('x, 'a))::Nil),
    Seq("a\n b;\n  c") -> App('a, mkBlock('b, 'c)),
    Seq("a\n +b;\n  c") -> bin('a, plus, mkBlock('b, 'c)),
    
    Seq("map ls\n f") -> App(App('map, 'ls), 'f),
    Seq("map\n ls\n  f") -> App('map, App('ls, 'f)),
  
    Seq("a.foo.bar", "a\n .foo\n .bar") -> OpAppL(OpAppL('a, foo), bar)
    // Note: "a"
  
  
  )
  
  "parsing invalid blocks" should "fail" in {
    intercept[Exception] { test("", null) }
    
    intercept[Exception] { test("a\n .foo\n  .bar", null) }
    /*    
     ┌ a
     │  .foo
     │   .bar  -- trying to apply .bar on .foo; probably not what we want
     └> [3.7] failure: end of input
    */
    
    intercept[Exception] { test("a\n b;\n c", null) }
    intercept[Exception] { test("a\n +b;\n c", null) }
  }

  "parsing lambdas" should "work" in tests(
    
    Seq("a => b", "a =>\n  b") -> Lambda('a.id -> 'b.id :: Nil),
  
    Seq("a => b | c => d" /*, "| a => b | c => d"*/) -> Lambda('a.id -> 'b.id :: 'c.id -> 'd.id :: Nil),
    
    Seq("a(b => c)", "a\n  b => c") -> App('a, Lambda('b.id -> 'c.id :: Nil)),
  
    Seq("a(b => c | e => f)") -> App('a, Lambda('b.id -> 'c.id :: 'e.id -> 'f.id :: Nil)),
  
    Seq(
      "a b => c d",
      "(a b) => (c d)",
      "a b =>\n  c d",
      "(a\n  b) => c d",
      "a  b => c\n  d"
    ) -> Lambda(App('a, 'b) -> App('c, 'd) :: Nil),
  
    Seq(
      "a =>\n  b\n  c"
    ) -> Lambda('a.id -> Block('b :: Nil, 'c))
  
  )
  
  "parsing multiline lambdas" should "work" in {
    val lsmap = App(OpAppL('ls, map), Lambda('a.id -> 'b.id :: 'c.id -> 'd.id :: Nil))
    
    tests(

    Seq(
"""
foo =
| b =>
  bar =
  | d => e
""") -> mkBlock(Let('foo, Lambda('b.id -> Block(Let('bar, Lambda('d.id -> 'e.id)) :: Nil)))),

    Seq(
"""
a
| b => c
  | d => e
""") -> App('a, Lambda('b.id -> App('c, Lambda('d.id -> 'e.id)))),

    Seq(
"""
ls.map
| a => b
| c => d
""","""
ls.map
  | a => b
  | c => d
""","""
ls.map
  | a =>
    b
  | c => d
""") -> lsmap,
    Seq(
"""
foo
| Some ls => ls.map
  | a => b
  | c => d
| None => Nil
""","""
foo
| Some ls =>
  ls.map
  | a => b
  | c => d
| None => Nil
""") -> App('foo, Lambda(App('Some,'ls) -> lsmap :: 'None.id -> 'Nil.id :: Nil))
    
)
  }
  
  
  
  "parsing lets" should "work" in tests(
    
    Seq("a = b", "a =\n  b") -> mkBlock(Let('a, 'b)),
    
    Seq("value a = b", "value a =\n  b") -> mkBlock(Let('a, 'b, Some(ValueModif))),
    
    Seq("type a = b", "type a =\n  b") -> mkBlock(Let('a, 'b, Some(TypeModif))),
  
    Seq("type a = b; type c = d", "type\n a = b\n c = d", "type\n  a = b\n  c = d") ->
      mkBlock(Let('a, 'b, Some(TypeModif)), Let('c, 'd, Some(TypeModif))),
  
    Seq("a = b => c | d => e","""
a =
| b => c
| d => e
""") -> mkBlock(Let('a, Lambda('b.id -> 'c.id, 'd.id -> 'e.id)))
  
  )
  
  "parsing ite" should "work" in tests(
    Seq("if (x > y) foo .else bar", """
if (x > y)
    foo
  .else
    bar
""","""
if (x > y)
  foo
.else
  bar
""") -> App(OpAppL(App(App('if, App(OpAppL('x, gt), 'y)), 'foo), els), 'bar),
  
    Seq("x > y ? foo .else bar", """
x > y ?
  foo
.else
  bar
""") -> App(OpAppL(App(OpAppL(App(OpAppL('x, gt), 'y), qmark), 'foo), els), 'bar)
  )
  
  
//  def test(str: Str, expected: Stmt) = parse(str, repl) match {
  def test(str: Str, expected: Term) = parse(str, pgrm) match {
    case Success(t, _) => assert(t == expected)
    case r @ NoSuccess(err, _) =>
      if (expected != null) println(r)
      throw new Exception("Failure: "+err)
  }
//  def tests(pairs: TestCase*) = {
//    var i = 0;
//    pairs foreach {p => (try p match {
//      case s: Single => (test _).tupled(s.pair)
//      case m: Multi => m.pairs._1 foreach (test(_, m.pairs._2))
//    } catch {
//      case e: Throwable =>
//        System.err.println(s"Test no.$i failed: "+p) //.pair._1)
//        throw e
//    }) oh_and (i += 1)}
//  }
  def tests(pairs: TestCase*) = {
    pairs foreach {
      case s: Single => try ((test _).tupled(s.pair)) catch {
        case e: Throwable =>
          System.err.println(s"Test failed: "+s.pair)
          throw e
      }
      case m: Multi => m.pairs._1 foreach (p => try test(p, m.pairs._2) catch {
        case e: Throwable =>
          System.err.println(s"Test failed: "+p)
          throw e
      })
    }
  }
  
  sealed trait TestCase
  implicit class Single(val pair: (Str, Term)) extends TestCase {
    override def toString = pair.toString
  }
  implicit class Multi(val pairs: (Seq[Str], Term)) extends TestCase {
    override def toString = pairs.toString
  }
  
  implicit class idable(s: Sym) {
    def id = Id(s)
  }
  
}


/*

Problems:

Operators by themselves...

    foo a + b    // ?
    foo a + b c  // ?
    
  Proposition 1:
  
      foo a + b    ==  foo (a + b)
      foo a + b c  ==  foo (a + b) c
  
    Pros:
      + fairly intuitive
    Cons:
      - there is already a syntax for prop1, which is to pack terms:
          foo a+b  ==  foo (a + b)
  
  Proposition 2:

      foo a + b    ==  (foo a) + b
      foo a + b c  ==  (foo a) + (b c)
    
    Pros:
      + permits nice paren-less separation of expressions
          Person "John" 42 register. db
      ++ it's both the OCaml and Haskell way, so I should probably pick this way for consistency with the ML family
      + it actually allows nice ITE syntaxes
          x > 0 ? "ok" .else "ko"
    Cons:
      - it may seem inconsistent when methods are viewed as functions..
          db.register Person "John" 42
          register (Person "John" 42)    -- this one needs parens!
        (on the other hand it can be and advantage of method syntax)
  
  
  Appendix: Implementation of both props
  
  Prop1:
  
    def term(st: State, multiLine: Bool): Parser[Term] = "term" !!! (rep1sep(
      compactTerm(st, multiLine) ~ rep1(air(operator) ~ compactTerm(st, multiLine).?) ^^ ReduceOps
    | compactTerm(st, multiLine)
    , space) <~ space.? ^^ { _ reduceLeft App })
  
  Prop2:
  
    def spaceAppsTerm(st: State, multiLine: Bool): Parser[Term] = "term" !!!
      (rep1sep(compactTerm(st, multiLine), space) <~ space.? ^^ { _ reduceLeft App })
    
    def term(st: State, multiLine: Bool): Parser[Term] = "term" !!! (rep1sep(
      spaceAppsTerm(st, multiLine) ~ rep(air(operator) ~ spaceAppsTerm(st, multiLine).?) ^^ ReduceOps
    , space) <~ space.? ^^ { _ reduceLeft App })
  



*/












