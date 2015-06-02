package front

import utils._
import common.Stage

trait Terms {
stage: Stage =>
  
  abstract class TermsTemplate {
//    import Terms._
    
    type DualWorld <: TermsTemplate
    val dualWorld: DualWorld
    
    type Wrap
    
    type Kind
    type SymbolType
    
    sealed trait Stmt
    
    case class Def(name: Sym, body: Term, dual: Boolean = false) extends Stmt
    
    sealed trait Term extends Stmt {
      def print = "..." // TODO
    }
    
  //  case class Dual(t: dualWorld.Term) extends Term
    case class Dependent(dep: dualWorld.Wrap, body: Wrap) extends Term
    
    case class App(fun: Wrap, arg: Wrap) extends Term
    
  //  case class DualApp(dual: DualMode[Term], f: Term, a: Term) extends Term
    case class DualApp(dualArg: DualMode[dualWorld.Wrap], f: Wrap, a: Wrap) extends Term
    
    type Arg = DualMode[Wrap]
    
    case class Lambda(arg: Arg, body: Wrap) extends Term
    
    case class Scope(stmts: Seq[Stmt], r: Wrap) extends Term
    
    case class Symbol(s: SymbolType) extends Term
    
    
    case class UnitTerm() extends Term
    
    
    
    
    
  }
  
  type Sym = Symbol
  
  trait DualMode[+T]
  
  sealed trait TypeKind
  case object StarKind extends TypeKind
  case object AtKind extends TypeKind
  case class Arrow(from: TypeKind, to: TypeKind) extends TypeKind
  
  object types extends TermsTemplate {
    type DualWorld = values.type
    val dualWorld = values
    type Wrap = Term
    type Kind = TypeKind
//    type SymbolType = TypeKindInfer.KindSchema
    type SymbolType = TypeKind
  }
  object values extends TermsTemplate {
    type DualWorld = types.type
    val dualWorld = types
    type Wrap = ValueWrap
    type Kind = Type
  }
  type Type = types.Term
  type Value = values.Term
  
  
//  val x = types.Dependent(values.UnitTerm(), types.UnitTerm())
  
  
  
//  sealed trait TypeTree
//  
//  case class Dependent(dep: Value, body: Type) extends TypeTree
//
//  case class RecordType(fields: Seq[(Sym, Type)]) extends TypeTree
//  
//  case class FunctionType(from: Type, to: Type) extends TypeTree
  
  
  
  
  


}















