package front

import utils._
import common._
import Stages._
import scala.util.{ Try, Success, Failure }
import collection._

abstract case class StageConverter[A <: Stage, B <: Stage](a: A, b: B) {
  import b._
  import b.values._

  val t = b.types
  val at = a.types
  val av = a.values

  type Ctx

  //  def apply(defs: Seq[a.Decl])(implicit c: Ctx): Seq[Decl] = defs map process

  /** Polymorphic definitions */

  //  def typs(x: a.TypSym)(implicit c: Ctx): b.TypSym
  //  def vals(x: a.ValSym)(implicit c: Ctx): b.ValSym
  def vnods(x: a.ValueNode)(implicit c: Ctx): b.ValueNode
  def tnods(x: a.TypeNode)(implicit c: Ctx): b.TypeNode

  //  def tspec(x: a.TypeSpec)(implicit c: Ctx):  b.TypeSpec
  //  def tparam(x: a.TypeParam)(implicit c: Ctx):  b.TypeParam

  /** Trees */

  def processTerm(tta: a.TermsTemplate, ttb: TermsTemplate)(x: tta.Term, syms: tta.Symbol => ttb.Symbol, nods: tta.Node => ttb.Node)(implicit c: Ctx): ttb.Term =
    {
      import tta._
      x match {
        case Unit()                            => ttb.Unit()
        case Literal(v)                        => ttb.Literal(v)
        case Ref(s)                            => ttb.Ref(syms(s))
        case App(f, a)                         => ttb.App(nods(f), nods(a))
        case Lambda(Extract(t), tta.Scoped(b)) => ttb.Lambda(ttb.Extract(nods(t)), ttb.Scoped(nods(b)))
        case Let(s, tta.Scoped(v))             => ttb.Let(syms(s), ttb.Scoped(nods(v)))
      }
    }
  def processVal(x: a.Value)(implicit c: Ctx): Value = (x: @unchecked) match {
    case x: a.values.Term => processTerm(a.values, b.values)(x, processValSym, vnods)
    case av.Ascribe(v, t) => Ascribe(vnods(v), tnods(t))
  }
  def processTyp(x: a.Type)(implicit c: Ctx): Type = (x: @unchecked) match {
    case x: a.types.Term => processTerm(a.types, b.types)(x, processTypSym, tnods)
  }

  def processValSym(x: a.ValueSymbol)(implicit c: Ctx): ValueSymbol = new ValueSymbol(x.name)
  def processTypSym(x: a.TypeSymbol)(implicit c: Ctx): TypeSymbol = new TypeSymbol(x.name)

  /** '@unchecked' is to avoid spurious non exhaustive match due to our weird cake pattern */
  def process(x: a.GeneralTerm)(implicit c: Ctx): GeneralTerm = (x: @unchecked) match {
    // Who needs a stack, right?
    // case x: a.Type  => process(x)
    // case x: a.Value => process(x)

    case x: a.Type  => processTyp(x)
    case x: a.Value => processVal(x)
  }

}

//case class SingleStaged(s: Stage) {
case class SingleStaged[S <: Stage](s: S) {
  abstract class Identity extends front.StageConverter[s.type, s.type](s, s) {

    //    def typs(x: a.TypSym)(implicit c: Ctx) = x
    //    def vals(x: a.ValSym)(implicit c: Ctx) = x
    def vnods(x: a.ValueNode)(implicit c: Ctx) = x
    def tnods(x: a.TypeNode)(implicit c: Ctx) = x

    def tspec(x: a.TypeSpec)(implicit c: Ctx) = x
    def tparam(x: a.TypeParam)(implicit c: Ctx) = x

  }
}

abstract class StageTraverser[S <: Stage](s: S) extends StageConverter[S, NilStage.type](s, NilStage) {

  ////  def typs(x: a.TypSym)(implicit c: Ctx) = null//.asInstanceOf[b.TypSym]
  ////  def vals(x: a.ValSym)(implicit c: Ctx) = null
  //  def vnods(x: a.ValueNode)(implicit c: Ctx) {} // { processVal(x.term) }
  //  def tnods(x: a.TypeNode)(implicit c: Ctx) {}

}

