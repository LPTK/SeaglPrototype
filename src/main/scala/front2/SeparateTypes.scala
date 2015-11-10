package front2

import utils._

import common._
import Stages2._


object SeparateTypes extends SameStageConverter[AST.type](AST) with Transformer {
  import Result._
  
//  def tstmt(x: a.TypeStmt): Result[b.TypeStmt] = tconv.process(x)
//  def vstmt(x: a.ValueStmt): Result[b.ValueStmt] = vconv.process(x)
  def mod(x: a.Modif): Result[b.Modif] = x
  
  //val tconv: TermsConverter[a.typez.type,b.typez.type] = new TermsConverter[a.typez.type,b.typez.type](AST.typez, AST.typez) {
  object tconv extends TermsConverterClass[a.typez.type,b.typez.type](AST.typez, AST.typez) with TypeConverter {
    
    //val co: TermsConverter[ta.dualWorld.type, tb.dualWorld.type] = vconv
    //val co: TermsConverter[AST.valuez.dualWorld.type, AST.valuez.dualWorld.type] = vconv
    //val co: TermsConverter[AST.valuez.DualWorld, AST.valuez.DualWorld] = vconv
    //val co: TermsConverter[a.valuez.type,b.valuez.type] = vconv
    
    def nod(x: ta.Node): Result[tb.Node] = tb.Node(process(x.term)).setPos(x.pos)
    def snod(x: ta.SubNode): Result[tb.SubNode] = nod(x)
    def kin(x: ta.Kind): Result[tb.Kind] = x
    
    def stmt(x: ta.Stmt) = process(x)
    
    //def mod(x: ta.Modif): Result[tb.Modif] = x
  }
  //val vconv: TermsConverter[a.valuez.type,b.valuez.type] = new TermsConverter[a.valuez.type,b.valuez.type](AST.valuez, AST.valuez) {
  object vconv extends TermsConverterClass[a.valuez.type,b.valuez.type](AST.valuez, AST.valuez) with ValueConverter {
    
    //val co: TermsConverter[ta.dualWorld.type, tb.dualWorld.type] = tconv
    //val co: TermsConverter[AST.typez.type, AST.typez.type] = tconv
    //val co: TermsConverter[AST.typez.DualWorld, AST.typez.DualWorld] = tconv
    //val co: TermsConverter[a.typez.type,b.typez.type] = tconv
    
    def nod(x: ta.Node): Result[tb.Node] = tb.Node(process(x.term)).setPos(x.pos)
    def snod(x: ta.SubNode): Result[tb.SubNode] = nod(x)
    def kin(x: ta.Kind): Result[tb.Kind] = x // TODO
    
    def stmt(x: ta.Stmt) = process(x)
    
    //def mod(x: ta.Modif): Result[tb.Modif] = x
    
  }
  
  
  
}
