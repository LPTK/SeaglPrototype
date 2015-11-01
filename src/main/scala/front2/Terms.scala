package front2

import utils._
import common._
import scala.collection.Seq

trait Terms {
  stage: Stage2 =>
  
  type AnyStmt = TypeStmt | ValueStmt
  implicit def t2stmt(a: TypeStmt): AnyStmt = Left(a)
  implicit def v2stmt(a: ValueStmt): AnyStmt = Right(a)
  
  
  /** TEMPLATE FOR CLASSES COMMON TO TYPES AND VALUES */
  abstract class TermsTemplate {
  terms =>
    
    type DualWorld <: TermsTemplate
    val dualWorld: DualWorld
    
    type Term
    type Kind
    type Metadata
    
    case class Node(term: Term, md: Metadata)
    type SubNode <: Node
    
    trait Stmt extends ASTStmt with CoreStmt
    
    /** Terms valid as the body of a `Let` (any term) */
    sealed trait GenTerm extends CoreTerm
    
    /** Terms valid as subexpressions (in ANF) */
    sealed trait SubTerm extends GenTerm with ASTTerm with CoreTerm
    
    //---
    // COMMON TERMS/STATEMENTS
    //---
    
    sealed trait Literal[T] extends SubTerm {
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
    
    case class Id(ident: Ident) extends SubTerm
    
    case class Atom(name: Sym, args: Ls[SubNode]) extends SubTerm
    
    //case class Tuple(first: TNode, second: TNode) extends SubTerm // first class or not..?
    
    case class App(fun: SubNode, arg: SubNode) extends GenTerm
    
    // case class Dual(t: dualWorld.Term) extends Term
    case class DepApp(fun: SubNode, darg: dualWorld.SubNode) extends GenTerm
    
    case class Block(stmts: Ls[AnyStmt], ret: Node) extends GenTerm
    
    case class Ascribe(v: SubNode, k: Kind) extends SubTerm
    
    
    case class Let(modif: Modif, pattern: Node, body: Node, where: Ls[AnyStmt] = Ls()) extends Stmt
    
    
    //---
    // AST TERMS/STATEMENTS
    //---
    
    sealed trait ASTTerm extends GenTerm
    sealed trait ASTStmt
    //object AST {
      
    case class Lambda(pattern: Node, body: Node) extends ASTTerm
    
    case class OpApp(arg: SubNode, op: Operator) extends ASTTerm
    
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
  
  
  /** Object instantiating type terms */
  object types extends TermsTemplate {
    type DualWorld = values.type
    lazy val dualWorld = values
    
    type Kind = Types.TypeKind
    
    type Term = Type
    
    type SubNode = TypeSubNode
    
    type Metadata = TypeMetadata
    
    /** TYPE-ONLY TERMS */
    
    //case class Ascribe(v: ValueNode, k: Types.TypeKind) extends types.ComTerm
    
  }
  
  /** Object instantiating value terms */
  object values extends TermsTemplate {
    type DualWorld = types.type
    val dualWorld = types
    
    type Kind = Type
    
    type Term = Value
    
    type SubNode = ValueSubNode
    
    type Metadata = ValueMetadata
    
    
    /** VALUE-ONLY TERMS */
    
    //case class Ascribe(v: ValueNode, t: TypeNode) extends values.ComTerm
    
    //case class Impure(n: Node) extends values.ComStmt
    
  }
  
  case class Impure(n: values.Node) extends values.Stmt
  
  /** ModBlock has a common definition for both types and values, so we provide a common extractor */ 
  object ModBlock {
    def unapply(x: TermsTemplate# ASTStmt) = x match {
      case types.ModBlock(modifs, stmts) => Some(modifs, stmts)
      case values.ModBlock(modifs, stmts) => Some(modifs, stmts)
    }
  }
  
}
