package common

import utils._
import front2._
import Printable._

trait Stage2 extends Terms {
  
  type Type
  type Value
  
  type TypeStmt
  type ValueStmt

  type TypeNode
  type ValueNode
  type LetValueNode
  
  //type UniqueSymbol
  
  implicit def typTerm(x: TypeNode): Type
  implicit def valTerm(x: ValueNode): Value
  
}

object Stages2 {
  import utils.Lazy

  // Main compilation stages:

  object AST extends Stage2 with PretypedStage {//with Scopes {
    
    type Type = types.ASTTerm
    type Value = values.ASTTerm
    
    type TypeStmt = types.ASTStmt
    type ValueStmt = values.ASTStmt
    
    type LetValueNode = ValueNode
    
    //case class ValueNode(term: Value, org: Origin) extends Originated
    //case class TypeNode(term: Value, org: Origin) extends Originated
    
  }
  
  // In ANF, ValueNode stores any node but App (stored in TopLevelValueNode)
  object ANF extends Stage2 with PretypedStage {
    
    type Type = types.CoreTerm
    type Value = values.CoreTerm
    
    type TypeStmt = types.CoreStmt
    type ValueStmt = values.CoreStmt
    
    type LetValueNode = values.LetTerm
    
  }
  
  object Typed extends Stage2 {
    
    case class ValueNode(term: Value, typ: Type, org: Origin) extends Originated
    case class TypeNode(term: Type, k: Types.TypeKind, org: Origin) extends Originated
    
    def typTerm(x: TypeNode): Type = x.term
    def valTerm(x: ValueNode): Value = x.term
    
  }
  

  // Common stage definitions:

  trait PretypedStage {
    self: Stage2 => //with Scopes =>
    
//    type TypeNode = Node[Type]
//    type ValueNode = Node[Value]
    case class ValueNode(term: Value, org: Origin) extends Originated
    case class TypeNode(term: Type, org: Origin) extends Originated
    //type TopLevelValueNode = ValueNode
      
    //case class Node[+T <: GeneralTerm](term: T, org: Origin)
    //case class Node[+T](term: T, org: Origin)
    
    def typTerm(x: TypeNode): Type = x.term
    def valTerm(x: ValueNode): Value = x.term
    
  }
  
  trait Originated {
    def org: Origin
  }
  
}
