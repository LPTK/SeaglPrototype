package common

import utils._
import front._
import Printable._

trait Stage extends Terms {

  type TypSym
  type ValSym

  type ValueNode
  type TypeNode //= Type // change if necessary in the future (+typeNodePrintable)

  implicit val valueNodePrintable: Printable[ValueNode]
  implicit val typeNodePrintable: Printable[TypeNode] = Printable { _ => "??" } // TODO

  type TypeSpec
  type TypeParam

  //  // TODOne use Printable type class instances instead
  //  def tname(s: TypSym): Str
  //  def vname(s: ValSym): Str
  //  def tpname(s: TypeParam): Str

}

object Stages {
  import utils.Lazy

  // Main compilation stages:

  object Ast extends Stage with PretypedStage {

    type TypSym = TId
    type ValSym = VId

    type TypeParam = TId
    
  }
  object Resolving extends Stage with PretypedStage {

    type TypSym = Lazy[TSym]
    type ValSym = Lazy[VSym]

    type TypeParam = Nothing // AbsTyp // TODO

    //    def tname(s: TypSym) = "??"
    //    def vname(s: ValSym) = "??"
    //    def tpname(s: TypeParam) = ??? // s.namStr // TODO

  }
  object Resolved extends Stage with PretypedStage with ResolvedStage {

  }
  //  object Typed extends Stage with ResolvedStage {
  //    import typing._
  //
  //    type ValueNode = Typed[Value]
  //
  //    type TypeSpec = Type
  //
  //  }

  // Common stage definitions:

  trait PretypedStage {
    self: Stage =>

    type ValueNode = Node[Value]
    type TypeNode = Node[Type]

    type TypeSpec = Opt[Type]
    
    case class Node[+T](term: T, org: Origin) {
      def to[U](s: PretypedStage)(f: T => U) = s.Node(f(term), org)
    }

    implicit val valueNodePrintable: Printable[ValueNode] =
      // Kinda hacky but works, except for ascribe
      Printable { x => values.termPrintable.print(x match { case x: values.Term => x })() }
  }

  trait ResolvedStage {
    self: Stage =>

    //    type TypSym = Cyclic[Type]
    //    type ValSym = Cyclic[Value]
    type TypSym = TSym
    type ValSym = VSym

    type TypeParam = Nothing // AbsTyp // TODO

    //    def tname(s: TypSym) = s.print()
    //    def vname(s: ValSym) = s.print()
    //    def tpname(s: TypeParam): Str = ??? // s.namStr // TODO

  }

  object NilStage extends Stage {

    type N = Nothing // Null

    type TypSym = N
    type ValSym = N

    type TypeParam = N

    type ValueNode = N

    type TypeSpec = N

    val valueNodePrintable: Printable[ValueNode] = ???

    def tname(s: TypSym) = ???
    def vname(s: ValSym) = ???
    def tpname(s: TypeParam) = ???
  }

}

