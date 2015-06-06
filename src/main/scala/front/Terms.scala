package front

import utils._
import common._

import Printable._

trait Terms {
  stage: Stage =>

  sealed trait Decl

  abstract class TermsTemplate {

    type DualWorld <: TermsTemplate
    val dualWorld: DualWorld

    type Node
    type Kind // TODO rm: not used/useful?
    type Sym

    //    sealed trait Stmt extends Printable
    //
    //    case class Def(sym: Sym, body: Node, dual: Boolean = false) extends Stmt {
    //      def print(implicit po: PrintOptions): Str = p"$sym = $body"
    //    }

    implicit val nodePrintable: Printable[Node]
    implicit val extPrintable: Printable[Extract[Nothing]] =
      Printable { ext => "`" + ext.x.print }
    /** TODO handle right parenthezsisaztion */
    implicit val termPrintable: Printable[Term] = Printable {
      case Literal(v)   => p"${v.toString}"
      case Ref(s)       => p"${s.toString}"
      case Let(s, v, b) => p"${s.toString} = $v; $b"
      case Lambda(a, b) => p"${a.toString} => $b" // FIXME rm toString
      case App(f, a)    => p"$f $a"
      case DepApp(f, a) => p"$f[${a.toString}]"
      //        case Ascribe(v, t) => p"$v: ${t.toString}"
    }

    sealed trait ValueTerm
    sealed trait TypeTerm

    sealed trait Term extends ValueTerm with TypeTerm {
      override def toString = this.print
    }

    case class Literal[T](value: T) extends Term

    case class Ref(sym: Sym) extends Term

    case class App(fun: Node, arg: Node) extends Term

    type Arg = Extract[Node] // FIXME: question: is Extraction really part of the core language?! (can be encoded using express)
    case class Lambda(arg: Arg, body: Node) extends Term

    case class Let(sym: Sym, value: Node, body: Node) extends Term with Decl

    //    case class Dual(t: dualWorld.Term) extends Term
    //    case class Dependent(dep: dualWorld.Node, body: Node) extends Term
    case class DepApp(fun: Node, darg: dualWorld.Node) extends Term

    //    case class Ascribe(v: Node, t: dualWorld.Node) extends ValueTerm
    case class Ascribe(v: ValueNode, t: Type) extends ValueTerm

    /** On another dimension than type/value, dual to expression world is extraction world */
    // trait DualMode[+T]
    case class Extract[+T](x: Node) {
      def print(implicit po: PrintOptions) = x.print
    }
    // TODO: should extraction really be a part of the core language?

  }

  sealed trait TypeKind
  case object StarKind extends TypeKind
  case object AtKind extends TypeKind
  case class Arrow(from: TypeKind, to: TypeKind) extends TypeKind

  object types extends TermsTemplate {
    type DualWorld = values.type
    lazy val dualWorld = values

    type Node = TypeNode //Term
    type Kind = TypeKind
    type Sym = TypSym

    val nodePrintable: Printable[Node] = Printable{_ => "??"} // TODO
  }
  object values extends TermsTemplate {
    type DualWorld = types.type
    val dualWorld = types

    type Node = ValueNode
    type Kind = Type
    type Sym = ValSym

    val nodePrintable: Printable[Node] = Printable{_ => "??"} // TODO
  }

  type Type = types.TypeTerm
  type Value = values.ValueTerm

}

