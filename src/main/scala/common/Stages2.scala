package common

import utils._
import front2._

import scala.util.parsing.input.Positional

/**
  * TODO: better org allowing more flexible, polymorphic use
  * Terms should define forms (AST, ANF) in parallel to phases/stages!
  *   they define the Term trait(!), nodes, and if necessary subnodes and subterms
  * Stage simply instantiates types/values (instead of Terms.. which is inappropriate) and their Metadata
  * 
  */
trait Stage2 extends Terms {
  
  type Type
  type Value
  
  type TypeStmt
  type ValueStmt
  
//  /** Info stored with each Node */
//  type TypeMetadata
//  type ValueMetadata
  
  type TypeNode
  type ValueNode
  
  /** Subnodes may refine TermsTemplate#Node with stricter bounds or additional info */
  //type TypeSubNode// <: types.Node
  type ValueSubNode// <: values.Node
  
  type Modif
  
}

object Stages2 {

  // Main compilation stages:

  object AST extends Stage2 with PretypedStage {
    
    type Type = types.ASTTerm
    type Value = values.ASTTerm
    
    type TypeStmt = types.ASTStmt
    type ValueStmt = values.ASTStmt
    
    class Pos extends Positional { def md = SourceCode(pos) }
    case class TypeNode(term: Type) extends Pos
    case class ValueNode(term: Value) extends Pos
    //type TypeSubNode = types.Node
    type ValueSubNode = values.Node
    
    type Modif = Ls[Modifier]
    
  }
  
  object ANF extends Stage2 with PretypedStage with ANFStage {
    
    case class TypeNode(term: Type, md: Origin)
    case class ValueNode(term: Value, md: Origin)
    
    class ValueSubNode(override val term: values.CoreTerm with values.SubTerm, md: Origin) extends ValueNode(term, md)
    
  }
  
  object Typed extends Stage2 with ANFStage {
    
//    case class TypeMetadata(typ: Type, org: Origin)
//    case class ValueMetadata(kin: Types.TypeKind, org: Origin)
    
  }
  

  // Common Stage-related definitions:

  trait PretypedStage {
//    type TypeMetadata = Origin
//    type ValueMetadata = Origin
  }
  
  /**
   * In ANF, App, OpApp and Block cannot be SubNodes, and have to be assigned to a temporary local variable instead.
   */
  trait ANFStage {
  self: Stage2 =>
    
    type Type = types.CoreTerm //types.GenTerm
    type Value = values.CoreTerm
    
    type TypeStmt = types.CoreStmt
    type ValueStmt = values.CoreStmt
    
    //type TypeSubNode = types.Node
    //class ValueSubNode(override val term: values.SubTerm, md: ValueMetadata) extends values.Node(term, md)
    //class ValueSubNode(override val term: values.SubTerm, md: ValueMetadata) extends ValueNode(term, md)
    
    type Modif = Modification
    
  }
  
  
}


