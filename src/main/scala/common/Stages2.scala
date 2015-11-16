package common

import common.doc.Document
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
  
//  type Type
//  type Value
//  
//  type TypeStmt
//  type ValueStmt
//  
////  /** Info stored with each Node */
////  type TypeMetadata
////  type ValueMetadata
//  
//  type TypeNode
//  type ValueNode
//  
//  /** Subnodes may refine TermsTemplate#Node with stricter bounds or additional info */
//  //type TypeSubNode// <: types.Node
//  type ValueSubNode// <: values.Node
//  
  type Modif
  
}

object Stages2 {
  
  // Main compilation stages:

  object AST extends Stage2 with PretypedStage with Common {
    
//    type Type = types.ASTTerm
//    type Value = values.ASTTerm
//    
//    type TypeStmt = types.ASTStmt
//    type ValueStmt = values.ASTStmt
//    
//    class Pos extends Positional { def md = SourceCode(pos) }
//    case class TypeNode(term: Type) extends Pos
//    case class ValueNode(term: Value) extends Pos
//    //type TypeSubNode = types.Node
//    type ValueSubNode = values.Node
//    
    type Modif = Ls[Modifier]
    
    object typez extends TermsTemplate with AST with ComTypes {
      type DualWorld = values.type
      lazy val dualWorld = values
      
      type Kind = Types.TypeKind
      
      //def stmt2anyS(a: Stmt) = Left(a)
      //def stmt2anyS = Left.apply
    }
//    object valuez extends TermsTemplate with AST with ComValues {
//      type DualWorld = types.type
//      val dualWorld = types
    object valuez extends DualTemplate[types.type](types) with AST with ComValues {
      
      //type Kind = Type
      type Kind = types.Node  
      
      //def stmt2anyS(a: Stmt) = Right(a)
      //def stmt2anyS = Right.apply
    }
    val values = valuez
    val types = typez
    
  }
  
  object Desugared extends Stage2 with PretypedStage with ANFStage {
    
    type TypeMetadata = Origin
    type ValueMetadata = Origin
    
//    case class TypeNode(term: Type, md: Origin)
//    case class ValueNode(term: Value, md: Origin)
//    
//    class ValueSubNode(override val term: values.CoreTerm with values.SubTerm, md: Origin) extends ValueNode(term, md)
    
  }
  
//  object Typed extends Stage2 with ANFStage {
//    
////    case class TypeMetadata(typ: Type, org: Origin)
////    case class ValueMetadata(kin: Types.TypeKind, org: Origin)
//    
//  }
  
  object Typed extends Stage2 with ANFStage {
    
    type TypeMetadata = Origin
    type ValueMetadata = (Origin, Type)
    
  }
  
  
  
  object Printed extends Stage2 with Common {
    
    object typez extends DocTerms with ComTypes {
      type DualWorld = values.type
      lazy val dualWorld = values
      
      //type Kind = Types.TypeKind
    }
    object valuez extends DualTemplate[types.type](types) with DocTerms with ComValues {
      //type Kind = Type
    }
    val values = valuez
    val types = typez
    
    trait DocTerms extends TermsTemplate {
      type DualWorld <: DocTerms
      
      type Node = Document
      def Node(term: Term, md: Metadata): Node = ???
      type SubNode = Node
      def SubNode(term: SubTerm with Term, md: Metadata): SubNode = ???
      
      type Kind = Document
      
      type Stmt = Nothing
    }
    
  }
  
  
  
  // Common Stage-related definitions:

  trait PretypedStage {
//    type TypeMetadata = Origin
//    type ValueMetadata = Origin
  }
  
  /**
   * In ANF, App, OpApp and Block cannot be SubNodes, and have to be assigned to a temporary local variable instead.
   */
  trait ANFStage extends Common {
  self: Stage2 =>
    
//    type Type = types.CoreTerm //types.GenTerm
//    type Value = values.CoreTerm
//    
//    type TypeStmt = types.CoreStmt
//    type ValueStmt = values.CoreStmt
//    
//    //type TypeSubNode = types.Node
//    //class ValueSubNode(override val term: values.SubTerm, md: ValueMetadata) extends values.Node(term, md)
//    //class ValueSubNode(override val term: values.SubTerm, md: ValueMetadata) extends ValueNode(term, md)
//    
    type Modif = Modification
    type TypeMetadata
    type ValueMetadata
    
    /** Note: cannot use Template[values.type](values) here because of cyclic dependency */
    object types extends Core with ComTypes {
      type DualWorld = values.type
      lazy val dualWorld = values
    //object types extends Template[values.type](values) with Core with ComTypes {
      
      type Metadata = TypeMetadata
      
      type Kind = Types.TypeKind
      
      type SubNode = Node
      def SubNode(term: SubTerm with Term, md: Metadata): SubNode = new SubNode(term, md)
      
      //def stmt2anyS(a: Stmt) = Left(a)
      //def stmt2anyS = Left.apply
    }
    
//    object values extends TermsTemplate with ANF with ComValues {
//      type DualWorld = types.type
//      val dualWorld = types
    object values extends DualTemplate[types.type](types) with ANF with ComValues {
      
      type Metadata = ValueMetadata
      
      //type Kind = Type
      type Kind = types.Node
      
      //def stmt2anyS(a: Stmt) = Right(a)
      //def stmt2anyS = Right.apply
    }
    
  }
  
  trait Common { // TODO use
  self: Stage2 =>
    
    abstract class DualTemplate[DW <: TermsTemplate](dw: DW) extends TermsTemplate {
      type DualWorld = DW
      lazy val dualWorld = dw
      
      //def stmt2anyS(a: Stmt) = stmt(a)
    }
    
    trait ComTypes { self: TermsTemplate =>
      def stmt2anyS = Left(_: Stmt)
    }
    trait ComValues { self: TermsTemplate =>
      def stmt2anyS = Right(_: Stmt)
    }
    
//    // doesn't work: dualWorld has incompatible type
//    trait ComValues2 { self: TermsTemplate =>
//      type DualWorld = types.type
//      lazy val dualWorld = types
//      def stmt2anyS = Right(_: Stmt)
//    }
    
  }
  
  
}


