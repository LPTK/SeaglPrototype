package front

import utils._
import common._

import Printable._

trait Terms {
  stage: Stage =>

  sealed trait GeneralTerm

  abstract class TermsTemplate {

    type DualWorld <: TermsTemplate
    val dualWorld: DualWorld

    type Node
    type Kind // TODO rm: not used/useful?
//    type Sym
    type TermId

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
      case Unit()       => p"()"
      case Literal(v)   => p"${v.toString}"
      case Ref(s)       => p"${s.toString}"
      case Let(s, v, b) => p"${s.toString} = ${v.node}; $b"
      case Lambda(a, b) => p"${a.toString} => ${b.node}" // FIXME rm toString
      case App(f, a)    => p"$f $a"
      case DepApp(f, a) => p"$f[${a.toString}]"
      //        case Ascribe(v, t) => p"$v: ${t.toString}"
    }

    sealed trait ValueTerm extends GeneralTerm
    sealed trait TypeTerm extends GeneralTerm

    sealed trait Term extends ValueTerm with TypeTerm {
      override def toString = this.print
    }

    case class Unit() extends Term

    case class Literal[T](value: T) extends Term

//    case class Ref(sym: Sym) extends Term
    case class Ref(sym: Symbol) extends Term

    case class App(fun: Node, arg: Node) extends Term

    type Arg = Extract[Node] // FIXME: question: is Extraction really part of the core language?! (can be encoded using express)
    case class Lambda(arg: Arg, body: Scoped) extends Term

    case class Let(sym: Symbol, value: Scoped, body: Node) extends Term

    //    case class Dual(t: dualWorld.Term) extends Term
    //    case class Dependent(dep: dualWorld.Node, body: Node) extends Term
    case class DepApp(fun: Node, darg: dualWorld.Node) extends Term

    case class Scoped(node: Node) extends ValueTerm
    
    //    case class Ascribe(v: Node, t: dualWorld.Node) extends ValueTerm
    case class Ascribe(v: ValueNode, t: TypeNode) extends ValueTerm

    /** On another dimension than type/value, dual to expression world is extraction world */
    // trait DualMode[+T]
    case class Extract[+T](x: Node) {
      def print(implicit po: PrintOptions) = x.print
    }
    // TODO: should extraction really be a part of the core language?

    class Symbol(val name: TermId) {
      private var _enclosingScope: Opt[Scope] = None
      protected[front] def scope(sc: Scope) =
        if (_enclosingScope.isDefined) throw new Error("Symbol's scope already defined") // TODO
        else _enclosingScope = Some(sc)
      
      def enclosingScope = _enclosingScope.get
      def apply() = ??? // TODO
    }
    
    
    
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
//    type Sym = TypSym
    type TermId = TId

    implicit val nodePrintable: Printable[Node] = stage.typeNodePrintable
  }
  object values extends TermsTemplate {
    type DualWorld = types.type
    val dualWorld = types

    type Node = ValueNode
    type Kind = Type
//    type Sym = ValSym
    type TermId = VId

    implicit val nodePrintable: Printable[Node] = stage.valueNodePrintable
  }

  type Type = types.TypeTerm
  type Value = values.ValueTerm

  type TypeSymbol = types.Symbol
  type ValueSymbol = values.Symbol
//  val TypeSymbol = types.Symbol
//  val ValueSymbol = values.Symbol
  
}









