package common

import utils._
import front2._
import Printable._

trait Stage2 extends Terms {
  
  type Type
  type Value
  
  type ValueLet
  
  type TypeStmt
  type ValueStmt

////  type TypeNode
////  type ValueNode
////  type LetValueNode
//  type TypeNode = Node[Type]
//  type ValueNode = Node[Value]
  
//  type Node[+T]
//  def typval[T](n: Node[T]): Node[Type] | Node[Value]
  
  type Metadata
  //case class Node[+T](term: T, md: Metadata)
//  case class TypeNode(term: Type, md: Metadata)
//  case class ValueNode(term: Value, md: Metadata)
  
  type TypeSubNode <: types.Node
  type ValueSubNode <: values.Node
  
  type Modif
  
  //type UniqueSymbol
  
//  implicit def typTerm(x: TypeNode): Type
//  implicit def valTerm(x: ValueNode): Value
  
}

object Stages2 {
  import utils.Lazy

  // Main compilation stages:

  object AST extends Stage2 with PretypedStage {//with Scopes {
    
    type Type = types.ASTTerm with types.GenTerm
    type Value = values.ASTTerm with values.GenTerm
    
    type TypeStmt = types.ASTStmt
    type ValueStmt = values.ASTStmt
    
    //type LetValueNode = ValueNode
    type TypeSubNode = types.Node
    type ValueSubNode = values.Node
    
    type Modif = Ls[Modifier]
    
//    type TypeTerm = types.ASTTerm
//    type ValueTerm = values.ASTTerm
    
    //case class ValueNode(term: Value, org: Origin) extends Originated
    //case class TypeNode(term: Value, org: Origin) extends Originated
    
    //sealed trait Node[+T]
    //case class Node[+T](term: types.LetTerm | values.LetTerm | T, org: Origin)
    
//    def typTerm(x: TypeNode): Type = x.term.fold(_.left.get, _)
//    def valTerm(x: ValueNode): Value = x.term
  }
  
  // In ANF, ValueNode stores any node but App (stored in TopLevelValueNode)
  object ANF extends Stage2 with PretypedStage {
    
    //type Type = types.CoreTerm
    type Type = types.GenTerm
    type Value = values.CoreTerm
    
    type TypeStmt = types.CoreStmt
    type ValueStmt = values.CoreStmt
    
    //type LetValueNode = values.LetTerm
    
    type TypeSubNode = types.Node
    //class ValueSubNode(override val term: values.CoreTerm, md: Metadata) extends values.Node(term, md)
    class ValueSubNode(override val term: values.SubTerm, md: Metadata) extends values.Node(term, md)
    
//    type ValueLet = values.App | values.DepApp
//    implicit def app2lett(a: values.App): ValueLet = Left(a)
//    implicit def depApp2lett(a: values.DepApp): ValueLet = Right(a)
    //type ValueLet = values.LetTerm
    
    
    type Modif = Modification
    
//    type TypeTerm = types.CoreTerm
//    type ValueTerm = values.CoreTerm
    
    
  }
  
//  object Typed extends Stage2 {
//    
//    case class ValueNode(term: Value, typ: Type, org: Origin) extends Originated
//    case class TypeNode(term: Type, k: Types.TypeKind, org: Origin) extends Originated
//    
//    def typTerm(x: TypeNode): Type = x.term
//    def valTerm(x: ValueNode): Value = x.term
//    
//  }
  

  // Common stage definitions:

  trait PretypedStage {
    self: Stage2 => //with Scopes =>
    
////    type TypeNode = Node[Type]
////    type ValueNode = Node[Value]
//    case class ValueNode(term: Value, org: Origin) extends Originated
//    case class TypeNode(term: Type, org: Origin) extends Originated
//    //type TopLevelValueNode = ValueNode
      
    //case class Node[+T <: GeneralTerm](term: T, org: Origin)
    //case class Node[+T](term: T, org: Origin)
    
//    case class Node[+T](term: T, org: Origin)
//    def typval[T](n: Node[T]) = n match {
//      case Node(_: Type, _) => Left(n.asInstanceOf[Node[Type]])
//      case Node(_: Value, _) => Right(n.asInstanceOf[Node[Value]])
//    }
    
//    def typTerm(x: TypeNode): Type = x.term
//    def valTerm(x: ValueNode): Value = x.term
    
    
    type Metadata = Origin
    
    
  }
  
  
}
