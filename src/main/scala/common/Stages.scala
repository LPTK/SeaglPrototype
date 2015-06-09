package common

import utils._
import front._
import Printable._

trait Stage extends Terms {

  type ValueNode
  type TypeNode

  type TypeSpec
  type TypeParam

  type Scope

  implicit val valueNodePrintable: Printable[ValueNode]
  implicit val typeNodePrintable: Printable[TypeNode]

}

object Stages {
  import utils.Lazy

  // Main compilation stages:

  object Ast extends Stage with PretypedStage with Scopes {

    //    type TypSym = TId
    //    type ValSym = VId

    type TypeParam = TId

  }
  //  object Resolving extends Stage with PretypedStage {
  //
  //    type TypSym = Lazy[TSym]
  //    type ValSym = Lazy[VSym]
  //
  //    type TypeParam = Nothing // AbsTyp // TODO
  //
  //    //    def tname(s: TypSym) = "??"
  //    //    def vname(s: ValSym) = "??"
  //    //    def tpname(s: TypeParam) = ??? // s.namStr // TODO
  //
  //  }
  //  object Resolved extends Stage with PretypedStage with ResolvedStage {
  //
  //  }
  //  //  object Typed extends Stage with ResolvedStage {
  //  //    import typing._
  //  //
  //  //    type ValueNode = Typed[Value]
  //  //
  //  //    type TypeSpec = Type
  //  //
  //  //  }

  // Common stage definitions:

  trait PretypedStage {
    self: Stage with Scopes =>

    type ValueNode = Node[Value]
    type TypeNode = Node[Type]

    type TypeSpec = Opt[Type]

    case class Node[+T <: GeneralTerm](term: T, org: Origin) extends Scope

    implicit val valueNodePrintable: Printable[ValueNode] =
      // Kinda hacky but works, except for ascribe
      Printable { x => values.termPrintable.print(x match { case Node(t: values.Term, org) => t })() }

    implicit val typeNodePrintable: Printable[TypeNode] =
      // Kinda hacky but works, except for ascribe
      Printable { x => types.termPrintable.print(x match { case Node(t: types.Term, org) => t })() }
  }

  //  trait ResolvedStage {
  //    self: Stage =>
  //
  //    //    type TypSym = Cyclic[Type]
  //    //    type ValSym = Cyclic[Value]
  //    type TypSym = TSym
  //    type ValSym = VSym
  //
  //    type TypeParam = Nothing // AbsTyp // TODO
  //
  //    //    def tname(s: TypSym) = s.print()
  //    //    def vname(s: ValSym) = s.print()
  //    //    def tpname(s: TypeParam): Str = ??? // s.namStr // TODO
  //
  //  }

  object NilStage extends Stage {

    type N = Unit

    type TypSym = N
    type ValSym = N

    type TypeParam = N

    type ValueNode = N
    type TypeNode = N

    type TypeSpec = N

    val valueNodePrintable: Printable[ValueNode] = Printable { _ => "()" }
    val typeNodePrintable: Printable[TypeNode] = Printable { _ => "()" }

  }

}
