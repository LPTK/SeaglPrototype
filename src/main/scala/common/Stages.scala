package common

import utils._
import front._


trait Stage extends Terms {
  
  type TypSym
  type ValSym
  
  type ValueNode
  type TypeNode = Type  // change if necessary in the future
  
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
    
//    def tname(s: TypSym) = s.toString
//    def vname(s: ValSym) = s.toString
//    def tpname(s: TypeParam) = s.toString
    
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
    
    type ValueNode = Value
    
    type TypeSpec = Opt[Type]
    
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
    
    def tname(s: TypSym) = ???
    def vname(s: ValSym) = ???
    def tpname(s: TypeParam) = ???
  }
  
}




















