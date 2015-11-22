package front2

import common.Printable.PrintOptions
import utils._
import common._
import scala.util.parsing.input.Positional

/**
  * TODO remove PrettyPrint & clean
  * TODO use Node for ops
  * 
  */
trait Terms extends PrettyPrint {
  stage: Stage2 =>
  
  import scala.language.implicitConversions
  
//  type AnyStmt = TypeStmt | ValueStmt
//  implicit def t2stmt(a: TypeStmt): AnyStmt = Left(a)
//  implicit def v2stmt(a: ValueStmt): AnyStmt = Right(a)
  type GenStmt = types.Stmt | values.Stmt
  /*implicit*/ def t2stmt(a: types.Stmt): GenStmt = Left(a)
  /*implicit*/ def v2stmt(a: values.Stmt): GenStmt = Right(a)
  
  
  /** TEMPLATE FOR CLASSES COMMON TO TYPES AND VALUES */
  trait TermsTemplate { //extends PrettyPrint {
  terms =>
    
    /** For some reason, does not always work (Scala does not see it and the warning remains) */
    trait ExplicitOuter { val outer = terms }
    
    val printer = Printer(this)
    import printer.PrettyPrinted
    
    type DualWorld <: TermsTemplate
    val dualWorld: DualWorld
    
    type Term
    type Stmt
    type Kind
    type Metadata
    //type Modif
    type Modif = stage.Modif
    
    //implicit def stmt2anyS(a: Stmt): AnyStmt //= Left(a)
    implicit def stmt2anyS: Stmt => GenStmt
    
    /**
     * If patterns are not to be put in ANF, Node[+T] would be more appropriate 
     */
    type Node //<: SelfPrintable //case class Node(term: Term, md: Metadata)
    def Node(term: Term, md: Metadata): Node
    type SubNode //<: SelfPrintable //<: Node
    def SubNode(term: SubTerm with Term, md: Metadata): SubNode
//    type NodeType = { val term: Term; val md: Metadata }
//    type Node <: NodeType //case class Node(term: Term, md: Metadata)
//    type SubNode <: NodeType //<: Node
//    val Node: (Term, Metadata) => Node
//    val SubNode: (SubTerm, Metadata) => SubNode
//    // Would also need a NodeTrait with terma and md...
    
    //trait Stmt extends ASTStmt with CoreStmt
    
    sealed trait AnyTerm
    sealed trait AnyStmt
    
    ///** Terms valid as the body of a `Let` (any term) */
    /** Terms that are BOTH AST and Core */
    sealed trait ComTerm extends ASTTerm with CoreTerm with ExplicitOuter
    
    /** Terms valid as subexpressions (in ANF) */
    sealed trait SubTerm extends AnyTerm //extends GenTerm //with ASTTerm with CoreTerm
    
    sealed trait ComStmt extends ASTStmt with CoreStmt { override val outer = terms }
    
    //---
    // COMMON TERMS/STATEMENTS
    //---
    
    sealed trait Literal[T] extends SubTerm with ComTerm {
      def value: T
    }
    object Literal {
      def apply[T](v: T) = v match {
        case () => UnitLit
        case v: Char => CharLit(v)
        case v: Str => StrLit(v)
        case v: Int => IntLit(v)
        case v: BigInt => IntLit(v)
        case v: Rational => RatLit(v)
      }
      def unapply(lit: Literal[_]) = Some(lit.value)
    }
    case object UnitLit extends Literal[Unit] { def value = () }
    case class CharLit(value: Char) extends Literal[Char]
    case class StrLit(value: Str) extends Literal[Str]
    case class IntLit(value: BigInt) extends Literal[BigInt]
    case class RatLit(value: Rational) extends Literal[Rational]
    
    case class Id(ident: Ident) extends SubTerm with ComTerm
    
    case class Atom(name: Sym, args: Ls[SubNode]) extends SubTerm with ComTerm
    
    //case class Tuple(first: TNode, second: TNode) extends SubTerm // first class or not..?
    
    case class App(fun: SubNode, arg: SubNode) extends ComTerm
    
    // case class Dual(t: dualWorld.Term) extends Term
    case class DepApp(fun: SubNode, darg: dualWorld.SubNode) extends ComTerm
    
    case class Block(stmts: Ls[GenStmt], ret: Node) extends ComTerm
    
    //case class Ascribe(v: SubNode, k: Kind) extends SubTerm with ComTerm
    case class Ascribe(v: SubNode, k: Kind) extends SubTerm with ComTerm
    
    
    case class Let(modif: Modif, pattern: Node, body: Node, where: Ls[GenStmt] = Ls()) extends ComStmt with ASTTerm
    
    /** TODO discard in type world at type-checking */
    case class Impure(n: Node) extends ComStmt //values.ComStmt// ComStmt with values.ComStmt
    
    
    //---
    // AST TERMS/STATEMENTS
    //---
    
    sealed trait ASTTerm extends AnyTerm with PrettyPrinted //extends SubTerm //extends GenTerm
    sealed trait ASTStmt extends AnyStmt with PrettyPrinted { val outer = terms }
    //object AST {
      
    case class Lambda(pattern: Node, body: Node) extends ASTTerm
    case class LambdaCompo(lambdas: Ls[Lambda]) extends ASTTerm {
      require(lambdas.size > 0)
    }
    
    case class OpAppL(arg: SubNode, op: Operator) extends ASTTerm
    
    case class OpAppR(op: Operator, arg: SubNode) extends ASTTerm
    
    case class OpTerm(op: Operator) extends ASTTerm
    
    case class ModBlock(modifs: Modif, stmts: Ls[GenStmt]) extends ASTStmt {
      require(stmts.size > 0)
    }
      
    //}
    
    
    //---
    // CORE TERMS/STATEMENTS
    //---
    
    sealed trait CoreTerm extends AnyTerm with PrettyPrinted
    sealed trait CoreStmt extends AnyStmt with PrettyPrinted { val outer = terms }
    //object Core {
      
    case class Closure(param: Ident, body: Node) extends CoreTerm with SubTerm
    
    case class RecBlock(stmts: Ls[GenStmt]) extends CoreStmt
      
    //}
    
    
    
  }
  
  trait AST extends TermsTemplate {
    type DualWorld <: AST
    type Metadata = SourceCode
    
    type Term = ASTTerm
    type Stmt = ASTStmt
    //type Modif = Ls[Modifier]
    
    //case class Node(term: Term) extends Positional with printer.PrettyPrinted { def md = SourceCode(pos) }
    case class Node(term: Term) extends Positional with printer.PrettyPrinted with SelfPrintable {
      def md = SourceCode(pos)
      def print(implicit po: PrintOptions) = Doc(term.toString, Metadata(md))
    }
    type SubNode = Node
//    val SubNode = Node
    def Node(term: Term, md: Metadata) = Node(term).setPos(md.pos)
    def SubNode(term: SubTerm with Term, md: Metadata) = Node(term, md)
  }
  
  trait Core extends TermsTemplate {
//    type Metadata = Origin // TODO change..? require method Metadata => common.Metadata
    
    type Term = CoreTerm
    type Stmt = CoreStmt
    //type Modif = Modification
    
    //case class Node(term: Term, md: Metadata) extends printer.PrettyPrinted with SelfPrintable {
    class Node(val term: Term, val md: Metadata) extends printer.PrettyPrinted with SelfPrintable {
      def print(implicit po: PrintOptions) = Doc("???") //Doc(term.toString, Metadata(md))
      override def equals(that: Any) = that match {
        //case Node(t, _) => term == t
        case x: Node => term == x.term
        case _ => false
      }
    }
    def Node(term: Term, md: Metadata): Node = new Node(term, md) // it's a shame Scala fails to see it implemented with Node a case class...
    
  }
  
  trait ANF extends Core {
    
    //case class Node(term: Term, md: Origin)
    class SubNode(override val term: CoreTerm with SubTerm, md: Metadata) extends Node(term, md) with printer.PrettyPrinted
    //def SubNode(term: CoreTerm with SubTerm, md: Metadata) = new SubNode(term, md)
    def SubNode(term: SubTerm with Term, md: Metadata): SubNode = new SubNode(term, md)
    
  }

  
  
  /** Object instantiating type terms */
  val types: TermsTemplate
//  object types extends TermsTemplate {
//    type DualWorld = values.type
//    lazy val dualWorld = values
//    
//    type Kind = Types.TypeKind
//    
//    type Term = Type
//    
//    type Node = TypeNode
//    type SubNode = Node //TypeSubNode
//    
//    //type Metadata = TypeMetadata
//    
//    /** TYPE-ONLY TERMS */
//    
//    //case class Ascribe(v: ValueNode, k: Types.TypeKind) extends types.ComTerm
//    
//  }
  
  /** Object instantiating value terms */
  val values: TermsTemplate
//  object values extends TermsTemplate {
//    type DualWorld = types.type
//    val dualWorld = types
//    
//    type Kind = Type
//    
//    type Term = Value
//    
//    type Node = ValueNode
//    type SubNode = ValueSubNode
//    
//    //type Metadata = ValueMetadata
//    
//    
//    /** VALUE-ONLY TERMS */
//    
//    //case class Ascribe(v: ValueNode, t: TypeNode) extends values.ComTerm
//    
//    //case class Impure(n: Node) extends values.ComStmt
//    
//  }
  
  //case class Impure(n: values.Node) extends values.ComStmt
  
  /** ModBlock has a common definition for both types and values, so we provide a common extractor */ 
  object ModBlock {
    def unapply(x: TermsTemplate# ASTStmt) = x match {
      case types.ModBlock(modifs, stmts) => Some(modifs, stmts)
      case values.ModBlock(modifs, stmts) => Some(modifs, stmts)
      case _ => None
    }
  }
  
  type Type = types.Term
  type Value = types.Term
  
  /**
    * @param typ the type expression
    * @param fa 'for all', universal type variables
    * @param fs 'for some', existential type variables
    * @param subt subtyping constraints
    * @param conc concept constraints
    */
  case class FullType(typ: Type, fa: Set[types.Id], fs: Set[types.Id], subt: Type -> Type |> Set, conc: ConceptApply |> Set)
  
  case class ConceptApply(conc: Type, args: Ls[Type])
  
}












