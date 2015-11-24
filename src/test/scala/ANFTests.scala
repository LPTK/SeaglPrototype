package parsing

import common._
import org.scalatest.FunSuite
import org.scalatest.{ FlatSpec, ShouldMatchers }

import utils._
import front2._  
import dsl._  

/**
  * TODO make 'anf' synthesize unique local idents
  * 
  */
class ANFTests extends FunSuite {
  import Stages2.Desugared.values._
  
  implicit class NodeEq(self: Node) {
    def <=> (that: Node): Bool = {
      val fva = this freeVars
      val fvb = that freeVars;
      if (fva =/= fvb) return false
      //eqtBindings(that) exists (_ exists {
      //  case a -> b => fva(a) && fvb(b)
      //} into not)
      eqtBindings(that) exists (_ forall {
        //case a -> b => (!fva(a) && !fvb(b)) 
        case a -> b => fva(a) ==> (fvb(b) && a === b) 
      })
      //} forall (_ == _))
    }
    def freeVars: Ident |> Set = {
      def rec(t: Term, bound: Set[Ident]): Ident |> Ls = t match {
        case Id(id) if !bound(id) => id :: Nil
        case Id(_) => Nil
        case Block(sts, r) =>
          //val bindings -> fv = sts flatMap {
          //  case Right(Let(_, pa, bo, _)) =>
          //    //val bindings = pa.freeVars
          //    //rec(bo.term, bound ++ bindings)
          //    pa.freeVars -> rec(bo.term)
          //  case _ => Nil
          //}
          //bs ++ rec(r, bs)
          var bindings, free = Set.empty[Ident];
          for (s <- sts) s match {
            case Right(Let(_, pa, bo, _)) =>
              free ++= rec(bo.term, bindings)
              bindings ++= pa.freeVars
            case Left(_) | Right(_) =>
          }
          rec(r term, bindings)
        case App(a, b, _) => rec(a term, bound) ++ rec(b term, bound)
        case Closure(pa, bo) => rec(bo term, bound + pa)
        case _: Literal[_] => Nil
        //case ... // TODO
      }
      rec(self term, Set empty) toSet
      //self.term |> rec toSet
      //self term into rec toSet
      //???
    }
    /** Bindings necessary to make the two nodes equivalent */
    def eqtBindings (that: Node): ?[Ident -> Ident |> Set] = self.term -> that.term match {
      case (a: Literal[_], b: Literal[_]) => if (a == b) Some(Set()) else None
      case Id(a) -> Id(b) => Set(a -> b) |> some
      case App(f1,a1,_) -> App(f2,a2,_) => for (f <- (f1 eqtBindings f2); a <- (a1 eqtBindings a2)) yield f ++ a
      case Block(sts1, r1) -> Block(sts2, r2) => for (sts <- Monad.sequence((sts1 zip sts2) map {
        case Right(Let(_, p1, b1, _)) -> Right(Let(_, p2, b2, _)) =>
          for (p <- (p1 eqtBindings p2); b <- (b1 eqtBindings b2)) yield p ++ b
        //case Left(_) | Right(_) => ???
        case _ => ???
      }); r <- r1 eqtBindings r2) yield sts.flatten.toSet ++ r
      case Closure(_, b1) -> Closure(_, b2) => for (b <- (b1 eqtBindings b2)) yield b
      case _ =>
        println("TODO" -> self.term -> that.term)
        //None
        ???
    }
    //def eqtBindings (that: SubNode): ?[Ident -> Ident |> Set] = eqtBindings(that)
  }
  
  test("trivial") {
    //val a = anf"lol"
    //val b = anf"((lol))"
    
    assert(anf"lol" == anf"((lol))")
    
  }
  
  test("app") {
    
    anfEqt(anf"a b c", anf"a b c")
    
    anfEqt(anf"a b c", anf"_x = a b; _x c")
    
    //anfEqt(anf"a b c", anf"_x = a b; _x d" |> not)
    assert(anf"a b c" <=> anf"_x = a b; _x d" |> not)
    
    anfEqt(anf"a b c d", anf"_x = a b; _y = _x c; _y d")
    
  }
  
  test("pat") {
    
    anfEqt(anf"a b c = e", anf"_x c = e; a b = _x")
    
    anfEqt(anf"a b c d = e", anf"_y d = e; _x c = _y; a b = _x")
    
  }
  
  test("lam") {
    
    anfEqt(anf"a b => c", anf"_x => (a b = _x; c)")
    
    anfEqt(anf"a b c => d e f", anf"_arg => ((_ab c) = _arg; (a b) = _ab; _de = (d e); (_de f))")
    
  }
  
  test("bloc") {
    
    anfEqt(anf"x => a b c", anf"x => (_x = a b; _x c)")
    
  }
  
  def anfEqt(a: Node, b: Node) = {
    if (a <=> b) ()
    else {
      val ap -> bp = DesugaredPrinter(a) -> DesugaredPrinter(b)
      System.err.println(s"-- Test failed:\n$ap\n-- FV = ${a.freeVars}\n$bp\n-- FV = ${b.freeVars}")
      assert(false)
    }
  }
  
  
  
}















