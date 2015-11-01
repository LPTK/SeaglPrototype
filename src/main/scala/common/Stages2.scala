package common

import utils._
import front2._

trait Stage2 extends Terms {
  
  type Type
  type Value
  
  type TypeStmt
  type ValueStmt
  
  /** Info stored with each Node */
  type Metadata
  
  /** Subnodes may refine TermsTemplate#Node with stricter bounds or additional info */
  type TypeSubNode <: types.Node
  type ValueSubNode <: values.Node
  
  type Modif
  
}

object Stages2 {

  // Main compilation stages:

  object AST extends Stage2 with PretypedStage {
    
    type Type = types.ASTTerm with types.GenTerm
    type Value = values.ASTTerm with values.GenTerm
    
    type TypeStmt = types.ASTStmt
    type ValueStmt = values.ASTStmt
    
    type TypeSubNode = types.Node
    type ValueSubNode = values.Node
    
    type Modif = Ls[Modifier]
    
  }
  
  object ANF extends Stage2 with PretypedStage with ANFStage {
    
  }
  
  object Typed extends Stage2 with ANFStage {
    
  }
  

  // Common Stage-related definitions:

  trait PretypedStage {
    type Metadata = Origin
  }
  
  /**
   * In ANF, App, OpApp and Block cannot be SubNodes, and have to be assigned to a temporary local variable instead.
   */
  trait ANFStage {
  self: Stage2 =>
    
    type Type = types.GenTerm
    type Value = values.CoreTerm
    
    type TypeStmt = types.CoreStmt
    type ValueStmt = values.CoreStmt
    
    type TypeSubNode = types.Node
    class ValueSubNode(override val term: values.SubTerm, md: Metadata) extends values.Node(term, md)
    
    type Modif = Modification
    
  }
  
  
}


