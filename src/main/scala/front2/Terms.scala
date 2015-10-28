package front2

import utils._
import common._
import scala.collection.Seq

trait Terms {
  stage: Stage2 =>
  
  
  //---
  // REQUIRED DEFINITIONS
  //---
    
////  type Type = types.ComTerm
////  type Value = values.ComTerm
//  type Type
//  type Value
//  
//  type TypeStmt
//  type ValueStmt
//  
////  type TypeSymbol = types.Symbol
////  type ValueSymbol = values.Symbol
//  //  val TypeSymbol = types.Symbol
//  //  val ValueSymbol = values.Symbol
  
  
  
  //---
  // PROVIDED DEFINITIONS
  //---
  
  
  type Stmt = TypeStmt | ValueStmt
  
  
  
  sealed trait Tree
  
  sealed trait GeneralTerm extends Tree // useful?
  
  /** TEMPLATE FOR CLASSES COMMON TO TYPES AND VALUES */
  abstract class TermsTemplate {
  terms =>
    
    type DualWorld <: TermsTemplate
    val dualWorld: DualWorld
    
    type Node
    type LetNode // In ANF, some nodes can only be let-nodes, and not sub-expressions
    type TermId
    type Kind
    
    sealed trait Ident
    case class Stable(path: Ls[Integer], name: Sym) extends Ident // eg: `Seagl :: Lang :: Int`
    class Local(val name: Opt[Sym] = None) extends Ident
    
    
    //sealed trait Stmt extends Tree
    sealed trait ComStmt extends ASTStmt with CoreStmt
    //type ComStmt = ASTStmt with CoreStmt
    
    
    //sealed trait Term extends GeneralTerm
    sealed trait LetTerm extends ASTTerm with CoreTerm with GeneralTerm
    //type ComTerm = ASTTerm with CoreTerm with GeneralTerm
    sealed trait ComTerm extends LetTerm
    
    //---
    // COMMON TERMS/STATEMENTS
    //---
    
    sealed trait Literal[T] extends ComTerm {
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
//    case class RatLit(numerator: BigInt, denominator: BigInt) extends Literal[TermsTemplate# RatLit] { //Literal[(BigInt,BigInt)] {
//      def value = this //(numerator, denominator)
//    }
    case class RatLit(value: Rational) extends Literal[Rational]
    
    case class Id(ident: Ident) extends ComTerm
    
    case class Atom(name: Sym, args: Ls[Node]) extends ComTerm
    
    case class Tuple(first: Node, second: Node) extends ComTerm // first class or not..?
    
    case class App(fun: Node, arg: Node) extends LetTerm
    
    case class Let(modifs: Ls[Modifier], pattern: Node, body: LetNode, where: Ls[Stmt]) extends ComStmt
    
    //    case class Dual(t: dualWorld.Term) extends Term
    case class DepApp(fun: Node, darg: dualWorld.Node) extends LetTerm
    
    case class Block(stmts: Seq[Stmt], ret: Node) extends ComTerm
    
    case class Ascribe(v: Node, k: Kind) extends ComTerm
    
    
    //---
    // AST TERMS/STATEMENTS
    //---
    
    sealed trait ASTTerm extends GeneralTerm
    sealed trait ASTStmt
    //object AST {
      
    case class Lambda(pattern: Node, body: Node) extends ASTTerm
    
    case class OpApp(arg: Node, op: Operator) extends ASTTerm
    
    case class ModBlock(modifs: Ls[Modifier], stmts: Seq[Stmt]) extends ASTStmt
      
    //}
    
    
    //---
    // CORE TERMS/STATEMENTS
    //---
    
    sealed trait CoreTerm extends GeneralTerm
    sealed trait CoreStmt
    //object Core {
      
    case class Closure(param: Ident, body: Node) extends CoreTerm
    
    case class RecBlock(stmts: Ls[Stmt]) extends CoreStmt
      
    //}
    
    
    
  }
  
  
  /** Object instantiating type terms */
  object types extends TermsTemplate {
    type DualWorld = values.type
    lazy val dualWorld = values

    type Node = TypeNode //Term
    type LetNode = Node
    type Kind = Types.TypeKind
    //    type Sym = TypSym
    type TermId = TId
    
    //type Stmt = TypeStmt
    
    
    /** TYPE-ONLY TERMS */
    
    //case class Ascribe(v: ValueNode, k: Types.TypeKind) extends types.ComTerm
    
  }
  
  /** Object instantiating value terms */
  object values extends TermsTemplate {
    type DualWorld = types.type
    val dualWorld = types

    type Node = ValueNode
    type LetNode = LetValueNode
    type Kind = Type
    //    type Sym = ValSym
    type TermId = VId
    
    //type Stmt = ValueStmt
    
    
    /** VALUE-ONLY TERMS */
    
    //case class Ascribe(v: ValueNode, t: TypeNode) extends values.ComTerm
    
    case class Impure(n: ValueNode) extends values.ComStmt
    
  }
  
}
