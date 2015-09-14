package parsing

import java.io.PrintStream
import java.nio.charset.Charset

import scala.io.StdIn._

import utils._

//object QuickTest extends App {println(Parser.parse("a"))}

//object QuickTest extends App {println(Parser.parse("a => b | c => d"))}

object QuickTest extends App {
  println(Parser.parse("a+b+c"))
  println(Parser.parse("a +b"))
}

//object QuickTest extends App {println(Parser.parse("a -- dsgvd,.;'\n\n", Parser.repl))}
//object QuickTest extends App {println(Parser.parse("a -- dsgvd,.;'", Parser.repl))}

//object QuickTest extends App {println(Parser.parse("""
//a
//  b
//    c
//    d
//  e
//"""))}

//object QuickTest extends App {println(Parser.parse("""
//a
//"""))}

//object QuickTest extends App {println(Parser.parse("""a"""))}



object ParserREPL extends App {
  val utf8 =
    try { System.setOut(new PrintStream(System.out, true, "utf-8")); true }
    catch { case _: SecurityException => Charset.defaultCharset().name == "UTF-8" }
  
  println("Parsing...");
  
//  println(Charset.defaultCharset().name())
  
  val (first, pre, post) = if (utf8)
      ("┌ ", "│ ", "└> ")
    else
      ("> ", "| ", "")
  
  val BreakOut = new Exception()
  
  try while (true) {
//    print("> ")
    // Doesn't print correctly in iTerm2; even the unicode encoding doesn't work... :(
    //   (http://ascii-table.com/ascii-extended-pc-list.php)
//    print("┌ ")
    print(first)
  
//    val line = readLine()
//  
//    // Internal block because we don't want to type delimiters at top level
//    val pgrm = Parser.pgrm(new Parser.lexical.Scanner(line))
//  
//    println(pgrm)
  
//    val pre = "| "
//    val pre = "│ "
    
//    val code = Iterate continually readLine takeWhile (_.nonEmpty) mkString "\n"
    val code = Iterate(readLine()) ++ (Iterate continually {print(pre); System.out.flush(); readLine()}
    ) map (str => if (str == null) throw BreakOut else str) takeWhile (_.nonEmpty) mkString "\n"
    
    val pgrm = Parser.pgrm(new Parser.lexical.Scanner(code))
    
    print("\b" * pre.length) // delete the last characters ("| ") -- doesn't seem to work on mac/idea 
//    println(pgrm)
    println(post + pgrm)
    
    
    // TODO: better handling of the parsed value
  
  } catch {
    case BreakOut =>
      println("Input closed.")
  }
  
}



object SeaglReader {
  import scala.util.parsing.input.CharArrayReader.EofCh
  
  def stdin: String = {
    val s = new StringBuilder();
    var c = System.in.read()
    
    while (c >= 0) {
      s.append(c.toChar)
      c = System.in.read()
    }
//    s.append(EofCh)
    
//    println(s.size)
    
    s.toString()
  }
  
  def main(args: Array[String]) {
    println(Parser.parse(stdin))
//    println(Parser.parse(stdin, Parser.repl))
  }
  
}
 
  
  
  
/*

Indentations without the intermission of an operator are taken as applications
  foo
    x = 1
    y = 2
    x + y

This is consistent with how operators/methods behave
  abc.foo  // operator, then indent = the block is the second argument
    x = 1
    y = 2
    x + y

Separating items in tuples and records needs no coma if newlines are used
  foo (
    arg1
    arg2
    arg3
  )

The semi-infinite paren can be used before an indent
  foo (|
    arg1
    arg2
    arg3

ALTERNATIVE:
Just use commas to define tuples
  foo (arg1, arg2, x = argx)
  foo (arg1, arg2, x = argx,)
  foo
    arg1, arg2, x = argx,
  foo
    arg1,
    arg2,
    x = argx,


Lines starting with | are taken as cases of a lambda; they already account for one indent
  ls.map
  | Some x => f x >>
    | Left x  => x
    | Right _ => 0
  | None   => 0

  // Note the definition of `>>` (like `|>`):
  a >> f = f a

Lambdas can be written in-line
  ls.map | Some x => f x >> (| Left x  => x | Right _ => 0) | None   => 0

Problem: conflict with semi-infinite paren!
We could require curly braces for lambdas; would be consistent with effectful by-name arguments syntax
  {Left x  => x | Right _ => 0}


Nesting can break out of indent
  foo
        a
      b
    c
  // eqto:
  foo a b c
Not to be abused, since not easy to read -- maybe just prevent it

Method/operator chaining (usefult; cf ITE)
  ls
    .add (Person "Bob" 42)
    .add
      Person
        "Bob"
        42
    .filter
      | Person age _ => age > 18
    .reverse



Examples:

  // Defining ITE:
  
  if true (f: -> _) = Some f
  if false _        = None
  
  (Some v).else f       = v
  (None).else (f: -> _) = f
  
  
  // Usage:
  
  if (p.age >= 18) "accepted" .else "rejected"
  
  if (p.age >= 18)
      print "cool!"
      "accepted"
    .else
      print "oops"
      "rejected"

  // Note: no-indent newline ops would allow a syntax a bit better (.else same indent as if):
  
  if (p.age >= 18)
    print "cool!"
    "accepted"
  .else
    print "oops"
    "rejected"

  // Alternative syntax, better separation between condition and branch:
  
  p.age >= 18 ? "accepted" .else "rejected"
  
  p.age >= 18 ?
    print "cool!"
    "accepted"
  .else
    print "oops"
    "rejected"
      
  //Not sure it's a good idea, but for "else" we could have something like:  
  
  p.age >= 18 ? "accepted" !? "rejected"
  p.age >= 18 ?
    "accepted"
  !?                     -- it's less legible
    "rejected"



  // Yet another way (could be used along with the previous one)
  
  if cond (then: -> _) (else: -> _) = cond >>
  | `True  => then
  | `False => else
  else = id
  
  // Note: smarter def for `if`:
  if cond (then: -> _) (else: -> _) = cond.fold then else
  
  // Usages:
  
  if (p.age >= 18) "accepted" "rejected"
  
  if (p.age >= 18)
      "accepted"
    else
      "rejected"
      
  // Not very nice syntax -- to avoid:
  if (p.age >= 18)
      "accepted"
    "rejected"  
  
  // Note: can be a disadvantage that else is the identity; giving it a special type may enhance user experience, even
  // if we can't use `if c t e` directly anymore, but rather `if c t e.else`


Problem of the approach...
.else if won't parse correctly

    if (x > y)
        foo
      .else if (x < y)
        bar
      .else
        baz
 
 // will parse like:
 
    ((if (x > y)
        foo
    ).else (if (x < y)
        bar)
    ).else
        baz

Solution: use an .elif function (just like in python)


Idea: use {} to escape pattern-mode and write in expression mode
Can be used to refer to variables; especially useful in type world!
  x = 42
  Some 42 >>
  | Some {x} => print "ok"
  | _        => ???

  type Foo =
  | {Int} => Long
  | T     => (T,T)

Problem: what about records?
Could use {{ double braces }} or ({mixed braces})


*/















