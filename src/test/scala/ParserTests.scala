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
    
//    tests("a" -> Id('a), "a " -> Id('a))
    tests(
      Seq("a", "a ", "a  ", "a--\n", "a -- ,.;'[(\n") -> 'a.id
    )
    
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
  
  val apb_pc = App(OpApp(App(OpApp('a, plus), 'b), plus), 'c)
  val ap_bpc = App(OpApp('a, plus), App(OpApp('b, plus), 'c))
  
  "parsing operator app" should "work" in tests(
    
    Seq("a + b", "a+ b", "a+b", "a\n +b", "a\n + b") -> App(OpApp('a, plus), 'b),
    Seq("a .foo b", "a.foo b", "a.foo(b)", "(a.foo) b", "(a .foo)b") -> App(OpApp('a, foo), 'b),
  
    Seq("a+b+c", "a+ b+ c", "a + b + c", "a \n + b\n  +c") -> apb_pc
    
  )
  
  "parsing newline operators" should "work" in tests(
  
    Seq("a + b + c", "a\n + b\n + c", "a\n +b\n +c") -> apb_pc,
  
    Seq("a + b+c", "a\n + b\n  + c") -> ap_bpc
    
  )
  
  "parsing blocks" should "work" in tests(
  
    Seq("a\n  x = b\n  x a", "a(\n x = b\n x a)") -> App('a, Block(Let('x, 'b)::Nil, App('x, 'a)))
  
  
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
    ) -> Lambda(App('a, 'b) -> App('c, 'd) :: Nil),
  
    Seq(
      "a =>\n  b\n  c"
    ) -> Lambda('a.id -> Block('b :: Nil, 'c))
  
  )
  
  "parsing multiline lambdas" should "work" in {
    val lsmap = App(OpApp('ls, map), Lambda('a.id -> 'b.id :: 'c.id -> 'd.id :: Nil))
    
tests(

//Seq("""
//foo =
//| b => bar =
//  | d => e
//""") -> Lambda('b.id -> Lambda('d.id -> 'e.id)),

Seq("""
a
| b => c
  | d => e
""") -> App('a, Lambda('b.id -> App('c, Lambda('d.id -> 'e.id)))),

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
  
  
  
  "parsing lets" should "work" in tests(
    
    Seq("a = b", "a =\n  b") -> Let('a, 'b),
  
    Seq("a = b => c | d => e","""
a =
| b => c
| d => e
""") -> Let('a, Lambda('b.id -> 'c.id, 'd.id -> 'e.id))
  
  )
  
  
  def test(str: Str, expected: Stmt) = parse(str, repl) match {
    case Success(t, _) => assert(t == expected)
    case r @ NoSuccess(err, _) =>
      println(r)
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
  implicit class Single(val pair: (Str, Stmt)) extends TestCase {
    override def toString = pair.toString
  }
  implicit class Multi(val pairs: (Seq[Str], Stmt)) extends TestCase {
    override def toString = pairs.toString
  }
  
  implicit class idable(s: Sym) {
    def id = Id(s)
  }
  
}














