package front2

import utils._
import common._
//import Stages._
import scala.util.{ Try, Success, Failure }
import collection._

abstract case class StageConverter[A <: Stage2, B <: Stage2](a: A, b: B) {
  import b._
  import b.values._

  val t = b.types
  val at = a.types
  val av = a.values

  type Ctx
  type Result[T]
  implicit val Result: Monad[Result]
  import Result._
  
  
  //  def apply(defs: Seq[a.Decl])(implicit c: Ctx): Seq[Decl] = defs map process
  
  /** Polymorphic definitions */

  //  def typs(x: a.TypSym)(implicit c: Ctx): b.TypSym
  //  def vals(x: a.ValSym)(implicit c: Ctx): b.ValSym
  def vnod(x: a.ValueNode)(implicit c: Ctx): Result[b.ValueNode]
  def tnod(x: a.TypeNode)(implicit c: Ctx): Result[b.TypeNode]
  
  
  def processComTerm(tta: a.TermsTemplate, ttb: TermsTemplate)
                 (x: tta.ComTerm, nods: tta.Node => Result[ttb.Node])
                 (implicit c: Ctx): Result[ttb.ComTerm] =
  { import tta._
    x match {
      case Literal(v) => ttb.Literal(v) |> lift
//      case App(f, a) => //ttb.App(nods(f), nods(a))
//        for (f <- nods(f); a <- nods(a)) yield ttb.App(f, a)
//        ???
    }
  }
  
//  def processVal(x: a.Value)(implicit c: Ctx): Value = (x/*: @unchecked*/) match {
//    case x: ComTerm => processComTerm(a.values, b.values)(x, vnods)
//    case a.Ascribe(v, t) => Ascribe(vnods(v), tnods(t))
//  }
  
  def processVal(x: a.Value)(implicit c: Ctx): Result[Value]
  
  def processTyp(x: a.Type)(implicit c: Ctx): Result[Type]
  
  
  
  
}

abstract class StageTraverser[A <: Stage2](a: A) extends StageConverter[A,A](a,a) {
  import a._
  
  type Result[T] = Monad.Discard[T]
  val Result = Monad.UnitMonad
  
//  def vnods(x: a.ValueNode)(implicit c: Ctx): Result[b.ValueNode]
//  def tnods(x: a.TypeNode)(implicit c: Ctx): Result[b.TypeNode]
  
  
}











