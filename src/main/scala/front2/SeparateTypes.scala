package front2

import utils._

import common._
import Stages2._


object SeparateTypes extends SameStageConverter[AST.type](AST) with Transformer {
  import Result._
  
  def tstmt(x: a.TypeStmt): Result[b.TypeStmt] = tconv.process(x)
  def vstmt(x: a.ValueStmt): Result[b.ValueStmt] = vconv.process(x)
  def mod(x: a.Modif): Result[b.Modif] = x
  
  val tconv: TermsConverter[a.types.type,b.types.type] = new TermsConverter[a.types.type,b.types.type](AST.types, AST.types) {
    
    val co: TermsConverter[ta.dualWorld.type, tb.dualWorld.type] = vconv
    
    def nod(x: ta.Node): Result[tb.Node] = tb.Node(process(x.term), x.md)
    def snod(x: ta.SubNode): Result[tb.SubNode] = nod(x)
    def kin(x: ta.Kind): Result[tb.Kind] = x
    
  }
  val vconv: TermsConverter[a.values.type,b.values.type] = new TermsConverter[a.values.type,b.values.type](AST.values, AST.values) {
    
    val co: TermsConverter[ta.dualWorld.type, tb.dualWorld.type] = tconv
    
    def nod(x: ta.Node): Result[tb.Node] = tb.Node(process(x.term), x.md)
    def snod(x: ta.SubNode): Result[tb.SubNode] = nod(x)
    def kin(x: ta.Kind): Result[tb.Kind] = x // TODO
    
  }
  
  
  
}
