package parsing

import org.scalatest.{ FlatSpec, ShouldMatchers }

import utils._
import Parser._
import AST._

class ParserTests extends FlatSpec with ShouldMatchers {
  
  implicit def sym2str(s: Sym): Str = s match { case Sym(str) => str }
  implicit def sym2id(s: Sym): Id = s match { case Sym(str) => Id(str) }
  
  val plus = Parser.lexical.SymbolOperator("+")
  val minus = Parser.lexical.SymbolOperator("-")
  
  val List(foo, bar, map) = List("foo", "bar", "map") map Parser.lexical.MethodOperator
  
  
  "parsing id" should "work" in {
    // Note: Starting with an indent is wrong, eg: " a"
    
    tests("a" -> Id('a), "a " -> Id('a))
    
  }
  
  "parsing app" should "work" in {
    
    tests(
    
      Seq("a b", "a(b)", "(a)b", "(a)(b)", "(a) (b)") -> App('a, 'b),
    
      Seq("a b c", "(a b)c") -> App(App('a, 'b), 'c),
    
      Seq("a (b c)", "a (b)(c)") -> App('a, App('b, 'c)),
    
      "a b c d" -> App(App(App('a, 'b), 'c), 'd),
      "a (b c) d" -> App(App('a, App('b, 'c)), 'd),
    
      "a b" -> App('a, 'b)
    )
    
  }

  "parsing partial operator app" should "work" in tests(
    
    Seq("a +", "a+", "(a+)") -> OpApp('a, plus),
    Seq("a b+", "a (b+)", "a(b +)") -> App('a, OpApp('b, plus)),
    Seq("a b+ c", "a (b+) c", "a(b +)c") -> App(App('a, OpApp('b, plus)), 'c)
    
  )
    
  "parsing operator app" should "work" in tests(
    
    Seq("a + b", "a+ b", "a+b") -> App(OpApp('a, plus), 'b),
    Seq("a .foo b", "a.foo b", "a.foo(b)", "(a.foo) b", "(a .foo)b") -> App(OpApp('a, foo), 'b)
    
  )
  
  "parsing blocks" should "work" in tests(
  
  )
  
  

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
    ) -> Lambda(App('a, 'b) -> App('c, 'd) :: Nil)
  
  )
  
  "parsing multiline lambdas" should "work" in {
    val lsmap = App(OpApp('ls, map), Lambda('a.id -> 'b.id :: 'c.id -> 'd.id :: Nil))
    
tests(
Seq("""
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
Seq(/*""" FIXME
foo
| Some ls => ls.map
  | a => b
  | c => d
| None => Nil
""",*/"""
foo
| Some ls =>
  ls.map
  | a => b
  | c => d
| None => Nil
""") -> App('foo, Lambda(App('Some,'ls) -> lsmap :: 'None.id -> 'Nil.id :: Nil))
    
)
  }
  
  
  def test(str: Str, expected: Stmt) = parse(str) match {
    case Success(t, _) => assert(t == expected)
    case r @ NoSuccess(err, _) =>
      println(r)
      throw new Exception("Failure: "+err)
  }
  def tests(pairs: TestCase*) = pairs foreach {
    case s: Single => (test _).tupled(s.pair)
    case m: Multi => m.pairs._1 foreach (test(_, m.pairs._2))
  }
  
  sealed trait TestCase
  implicit class Single(val pair: (Str, Stmt)) extends TestCase
  implicit class Multi(val pairs: (Seq[Str], Stmt)) extends TestCase
  
  implicit class idable(s: Sym) {
    def id = Id(s)
  }
  
}














