package front2

import utils._
import common._
import scala.collection.Seq

trait Terms {
  stage: Stage2 =>
  
  
  //---
  // REQUIRED DEFINITIONS
  //---
  
  // Moved to Stage2
  
  
  
  //---
  // PROVIDED DEFINITIONS
  //---
  
  
  type Stmt = TypeStmt | ValueStmt
  implicit def t2stmt(a: TypeStmt): Stmt = Left(a)
  implicit def v2stmt(a: ValueStmt): Stmt = Right(a)
  
  
  sealed trait Tree
  
  sealed trait GeneralTerm extends Tree // useful?
  
  /** TEMPLATE FOR CLASSES COMMON TO TYPES AND VALUES */
  abstract class TermsTemplate {
  terms =>
    
    type DualWorld <: TermsTemplate
    val dualWorld: DualWorld
    
    //type Node
    type Term
    //type LetNode // In ANF, some nodes can only be let-nodes, and not sub-expressions
    //type LetTerm // In ANF, some nodes can only be let-nodes, and not sub-expressions
    type TermId
    type Kind
    
    //case class Node[+T <: LetTerm](term: T, md: Metadata)
    case class Node(term: Term, md: Metadata)
    //case class SubNode(term: T, md: Metadata)
    //class SubNode(override val term: T, md: Metadata)
    type SubNode <: Node
      
    type TNode = SubNode //[Term]
    
//    sealed trait Ident
//    case class Stable(path: Ls[Integer], name: Sym) extends Ident // eg: `Seagl :: Lang :: Int`
//    class Local(val name: Sym) extends Ident
//    class Synthetic(val nameHint: Opt[Sym] = None) extends Ident
    
    
    //sealed trait Stmt extends Tree
     trait ComStmt extends ASTStmt with CoreStmt
    //type ComStmt = ASTStmt with CoreStmt
    
    
//    //sealed trait Term extends GeneralTerm
//    sealed trait LetTerm extends ASTTerm with CoreTerm with GeneralTerm
//    //type ComTerm = ASTTerm with CoreTerm with GeneralTerm
//    sealed trait ComTerm extends LetTerm
//    //sealed trait ComTerm extends ASTTerm with CoreTerm with GeneralTerm
    /** Terms valid as the body of a `Let` */
    sealed trait GenTerm extends CoreTerm
    /** Terms valid as subexpressions */
    sealed trait SubTerm extends GenTerm with ASTTerm with CoreTerm with GeneralTerm
    
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
    
    case class Atom(name: Sym, args: Ls[TNode]) extends SubTerm
    
    //case class Tuple(first: TNode, second: TNode) extends SubTerm // first class or not..?
    
    case class App(fun: TNode, arg: TNode) extends GenTerm
    
    // case class Dual(t: dualWorld.Term) extends Term
    case class DepApp(fun: TNode, darg: dualWorld.SubNode) extends GenTerm
    
    case class Block(stmts: Ls[Stmt], ret: Node) extends GenTerm
    
    case class Ascribe(v: TNode, k: Kind) extends SubTerm
    
    
    case class Let(modif: Modif, pattern: Node, body: Node, where: Ls[Stmt] = Ls()) extends ComStmt
    
    
    //---
    // AST TERMS/STATEMENTS
    //---
    
    sealed trait ASTTerm extends GenTerm //extends GeneralTerm
    sealed trait ASTStmt
    //object AST {
      
    case class Lambda(pattern: Node, body: Node) extends ASTTerm
    
    case class OpApp(arg: TNode, op: Operator) extends ASTTerm
    
    case class ModBlock(modifs: Modif, stmts: Ls[Stmt]) extends ASTStmt {
      require(stmts.size > 0)
    }
      
    //}
    
    
    //---
    // CORE TERMS/STATEMENTS
    //---
    
    sealed trait CoreTerm //extends GenTerm //GeneralTerm
    sealed trait CoreStmt
    //object Core {
      
    case class Closure(param: Ident, body: Node) extends CoreTerm with SubTerm
    
    case class RecBlock(stmts: Ls[Stmt]) extends CoreStmt
      
    //}
    
    
    
  }
  
  
  /** Object instantiating type terms */
  object types extends TermsTemplate {
    type DualWorld = values.type
    lazy val dualWorld = values

    //type Node = TypeNode //Term
    //type LetNode = Node
    type Kind = Types.TypeKind
    //    type Sym = TypSym
    type TermId = TId
    
    //type Stmt = TypeStmt
    
    type Term = Type
    //type LetTerm = 
    
    type SubNode = TypeSubNode
    
    
    /** TYPE-ONLY TERMS */
    
    //case class Ascribe(v: ValueNode, k: Types.TypeKind) extends types.ComTerm
    
  }
  
  /** Object instantiating value terms */
  object values extends TermsTemplate {
    type DualWorld = types.type
    val dualWorld = types

    //type Node = ValueNode
    //type LetNode = LetValueNode
    type Kind = Type
    //    type Sym = ValSym
    //type TermId = VId
    
    //type Stmt = ValueStmt
    
    type Term = Value
    //type LetTerm = ValueLet
    
    type SubNode = ValueSubNode
    
    
    /** VALUE-ONLY TERMS */
    
    //case class Ascribe(v: ValueNode, t: TypeNode) extends values.ComTerm
    
    //case class Impure(n: Node) extends values.ComStmt
    
  }
  
  case class Impure(n: values.Node) extends values.ComStmt
  
  // ModBlock
  object ModBlock {
    //def unapply(mb: types.ModBlock) = Some(mb.modifs, mb.stmts)
    def unapply(x: TermsTemplate# ASTStmt) = x match {
      case types.ModBlock(modifs, stmts) => Some(modifs, stmts)
      case values.ModBlock(modifs, stmts) => Some(modifs, stmts)
    }
  }
  
}
