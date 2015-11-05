package front2

import utils._
import common._
import scala.collection.Seq
import scala.util.parsing.input.Positional

trait Terms {
  stage: Stage2 =>
  
  import scala.language.implicitConversions
  
//  type AnyStmt = TypeStmt | ValueStmt
//  implicit def t2stmt(a: TypeStmt): AnyStmt = Left(a)
//  implicit def v2stmt(a: ValueStmt): AnyStmt = Right(a)
  type AnyStmt = types.Stmt | values.Stmt
  implicit def t2stmt(a: types.Stmt): AnyStmt = Left(a)
  implicit def v2stmt(a: values.Stmt): AnyStmt = Right(a)
  
  
  /** TEMPLATE FOR CLASSES COMMON TO TYPES AND VALUES */
  trait TermsTemplate {
  terms =>
    
    type DualWorld <: TermsTemplate
    val dualWorld: DualWorld
    
    type Term
    type Stmt
    type Kind
    type Metadata
    //type Modif
    
    /**
     * If patterns are not to be put in ANF, Node[+T] would be more appropriate 
     */
    type Node //case class Node(term: Term, md: Metadata)
    type SubNode //<: Node
//    type NodeType = { val term: Term; val md: Metadata }
//    type Node <: NodeType //case class Node(term: Term, md: Metadata)
//    type SubNode <: NodeType //<: Node
//    val Node: (Term, Metadata) => Node
//    val SubNode: (SubTerm, Metadata) => SubNode
//    // Would also need a NodeTrait with terma and md...
    
    //trait Stmt extends ASTStmt with CoreStmt
    
    ///** Terms valid as the body of a `Let` (any term) */
    /** Terms that are BOTH AST and Core */
    sealed trait ComTerm extends ASTTerm with CoreTerm
    
    /** Terms valid as subexpressions (in ANF) */
    sealed trait SubTerm //extends GenTerm //with ASTTerm with CoreTerm
    
    sealed trait ComStmt extends ASTStmt with CoreStmt
    
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
    
    case class Block(stmts: Ls[AnyStmt], ret: Node) extends ComTerm
    
    case class Ascribe(v: SubNode, k: Kind) extends SubTerm with ComTerm
    
    
    case class Let(modif: Modif, pattern: Node, body: Node, where: Ls[AnyStmt] = Ls()) extends ComStmt with ASTTerm
    
    
    //---
    // AST TERMS/STATEMENTS
    //---
    
    sealed trait ASTTerm //extends SubTerm //extends GenTerm
    sealed trait ASTStmt
    //object AST {
      
    case class Lambda(pattern: Node, body: Node) extends ASTTerm
    case class LambdaCompo(lambdas: Ls[Lambda]) extends ASTTerm
    
    case class OpAppL(arg: SubNode, op: Operator) extends ASTTerm
    
    case class OpAppR(op: Operator, arg: SubNode) extends ASTTerm
    
    case class OpTerm(op: Operator) extends ASTTerm
    
    case class ModBlock(modifs: Modif, stmts: Ls[AnyStmt]) extends ASTStmt {
      require(stmts.size > 0)
    }
      
    //}
    
    
    //---
    // CORE TERMS/STATEMENTS
    //---
    
    sealed trait CoreTerm
    sealed trait CoreStmt
    //object Core {
      
    case class Closure(param: Ident, body: Node) extends CoreTerm with SubTerm
    
    case class RecBlock(stmts: Ls[AnyStmt]) extends CoreStmt
      
    //}
    
    
    
  }
  
  trait AST extends TermsTemplate {
    type DualWorld <: AST
    
    type Term = ASTTerm
    type Stmt = ASTStmt
    //type Modif = Ls[Modifier]
    
    case class Node(term: Term) extends Positional { def md = SourceCode(pos) }
    type SubNode = Node
    
  }
  
  trait Core extends TermsTemplate {
    
    type Term = CoreTerm
    type Stmt = CoreStmt
    //type Modif = Modification
    
    case class Node(term: Term, md: Origin)
    
  }
  
  trait ANF extends Core {
    
    //case class Node(term: Term, md: Origin)
    class SubNode(override val term: CoreTerm with SubTerm, md: Origin) extends Node(term, md)
    
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
  
  case class Impure(n: values.Node) extends values.ComStmt
  
  /** ModBlock has a common definition for both types and values, so we provide a common extractor */ 
  object ModBlock {
    def unapply(x: TermsTemplate# ASTStmt) = x match {
      case types.ModBlock(modifs, stmts) => Some(modifs, stmts)
      case values.ModBlock(modifs, stmts) => Some(modifs, stmts)
    }
  }
  
  type Type = types.Term
  type Value = types.Term
  
}
