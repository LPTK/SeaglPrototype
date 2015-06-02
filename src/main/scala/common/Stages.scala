package common

import utils._
import front._


trait Stage extends Terms {
  
  type TypSym
  type ValSym
  
  type ValueWrap //<: Stmt
  
  type Decl
  
  type TypeSpec
  type TypeParam
  
  
  def tname(s: TypSym): Str
  def vname(s: ValSym): Str
  def tpname(s: TypeParam): Str  
  
}


object Stages {
  import utils.Lazy
  import scala.util.Try
  
  trait Pretyped {
  self: Stage =>
    
    type ValueWrap = Value
    
    type TypeSpec = Opt[Type]
    
  }
  
  object Ast extends Stage with Pretyped {
    
    type TypSym = TId
    type ValSym = VId
    
    type TypeParam = TId
    
    def tname(s: TypSym) = s.toString
    def vname(s: ValSym) = s.toString
    def tpname(s: TypeParam) = s.toString
    
  }
  
  
  object Resolving extends Stage with Pretyped {
    
    type TypSym = Lazy[Type]
    type ValSym = Lazy[Value]
    
    type TypeParam = Nothing // AbsTyp // TODO
    
    def tname(s: TypSym) = "??"
    def vname(s: ValSym) = "??"
    def tpname(s: TypeParam) = ??? // s.namStr // TODO
    
  }
  
  trait ResolvedStage {
  self: Stage =>
    
    type TypSym = Cyclic[Type]
    type ValSym = Cyclic[Value]
    
    type TypeParam = Nothing // AbsTyp // TODO
    
    
    def tname(s: TypSym) = s.value.print
    def vname(s: ValSym) = if (s.wasComputerYet) s.value.print.toString else "??"
    def tpname(s: TypeParam): Str = ??? // s.namStr // TODO
    
  }
  
  object Resolved extends Stage with Pretyped with ResolvedStage {
    
  }
  
//  object Typed extends Stage with ResolvedStage {
//    import typing._
//    
//    type ValueWrap = Typd[BasicExpr]
//    
//    type TypeSpec = Type
//    
//  }
  
  
  
  
  
  object NilStage extends Stage {
    
    type N = Nothing // Null
    
    type TypSym = N
    type ValSym = N
    
    type TypeParam = N
    
    type ValueWrap = N
    
    type TypeSpec = N
    
    def tname(s: TypSym) = ???
    def vname(s: ValSym) = ???
    def tpname(s: TypeParam) = ???
  }
  
}




















