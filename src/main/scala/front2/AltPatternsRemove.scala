package front2

import utils._

import common._
import Stages2.AST

/**
  * TODO implement and use this class
  * TODO perhaps this should be merged with ToANF?
  * TODO generalize and use to replace LocalId with SyntheticId
  * 
  * Removes patterns with alternatives, transforming them into extractions through lambdas.
  * For example:
  *   (Some x, None, y | None, Some x, y) => x + y
  * ->
  *   arg =>
  *     (x,y) = ((Some x, None, y => x,y) | (None, Some x => x,y)) arg
  *     x + 1
  * 
  */
//object AltPatternsRemove extends StageConverter[AST.type, AST.type](AST, AST) with ScopingConverter[] {
object AltPatternsRemove extends SameStageConverterClass[AST.type](AST) with ScopingConverter[AST.type,AST.type] with Transformer {
  
  def updateCtx[A](x: Result[A])(f: Ctx => Ctx): Result[A] = ???
  
  def regTyp(id: Ident): TypeSymbol = ???
  def regVal(id: Ident): ValueSymbol = ???
  
  def mod(x: a.Modif): Result[b.Modif] = x
  
  
  //object tconv extends ScopingTermsConverter[AST.types.type, AST.values.type](AST.values, AST.types) {
  object tconv extends TermsConverterClass[AST.types.type, AST.types.type](AST.types, AST.types)
    with TypeConverter
    with ScopingTermsConverter[AST.types.type, AST.types.type]
  {
    
    def nod(x: ta.Node): Result[tb.Node] = tb.Node(process(x.term)).setPos(x.pos)
    def snod(x: ta.SubNode): Result[tb.SubNode] = nod(x)
    def kin(x: ta.Kind): Result[tb.Kind] = x
    
    def stmt(x: ta.Stmt) = process(x)
    
  }
  
  object vconv extends TermsConverterClass[AST.values.type, AST.values.type](AST.values, AST.values)
    with ValueConverter
    with ScopingTermsConverter[AST.values.type, AST.values.type]
  {
    
    def nod(x: ta.Node): Result[tb.Node] = tb.Node(process(x.term)).setPos(x.pos)
    def snod(x: ta.SubNode): Result[tb.SubNode] = nod(x)
    def kin(x: ta.Kind): Result[tb.Kind] = x
    
    def stmt(x: ta.Stmt) = process(x)
    
    import ta._
    
  }
  
}
