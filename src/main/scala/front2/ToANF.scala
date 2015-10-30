package front2

import utils._

import common._
import Stages2._


class ToANF extends StageConverter[AST.type, ANF.type](AST, ANF) {
  
  val phaseName = "ToANF"
  
  type Result[T] = Monad.Simple[T]
  val Result = Monad.Simple
  
  //def tstmt(x: a.TypeStmt): Result[b.TypeStmt] = tconv.process(x)
  //def tstmt(x: a.TypeStmt): Result[ANF.types.CoreStmt] = tconv.process(x: AST.types.ASTStmt)//: Result[ANF.types.CoreStmt]
  //def tstmt(x: a.TypeStmt): Result[ANF.types.CoreStmt] = tconv.processC(x: tconv.ta.ASTStmt): Result[ANF.types.CoreStmt]
  def tstmt(x: a.TypeStmt): Result[b.TypeStmt] = x match {
    case a.types.ModBlock(mo, sts) =>
      // TODO check mo; make type or value
      // TODO what to do when NOT creating a RecBlock??
      ???
  }
  def vstmt(x: a.ValueStmt): Result[b.ValueStmt] = ???
  def mod(x: a.Modif): Result[b.Modif] = Modification(x contains Priv)
  
  val tconv: TermsConverter[AST.types.type,ANF.types.type] = new AnfTermsConverter[AST.types.type,ANF.types.type](AST.types, ANF.types) {
    val co = vconv
    def nod(x: ta.Node): Result[tb.Node] = tb.Node(x.term match { //tb.Node(processA(x.term), x.md)
      //case ta.Lambda(pa, bo) => ???
      case ta.Lambda(id: Ident, bo) => tb.Closure(id, nod(bo))
      case ta.Lambda(pa, bo) =>
        val id = new SyntheticId(nameHint(pa.term))
        val idn = tb.Node(tb.Id(id): tb.Term, Synthetic(phaseName, pa.md))
        val let = tb.Let(Modification(false), nod(pa), idn)
        tb.Closure(id, mkBlock(let)(nod(bo)))
      case ta.OpApp(ar, op) => tb.App(tb.Node(tb.Id(op.id), x.md/*TODO better md*/), nod(ar))
    }, x.md)
    
//    def snod(x: ta.SubNode): Result[tb.SubNode] = new b.ValueSubNode(x.term match { // TODO: how to properly convert a nested App?
//      case _ => ???
//    }, x.md)
    def snod(x: ta.SubNode): Result[tb.SubNode] = tb.Node(process(x.term), x.md)
    
  }
  val vconv: TermsConverter[a.values.type,b.values.type] = ???
  
  abstract class AnfTermsConverter[
    TA <: a.TermsTemplate,
    TB <: b.TermsTemplate { type Term = TB# GenTerm; type Kind = TA# Kind }
  ](val _ta: TA, val _tb: TB) extends TermsConverter[TA,TB](_ta,_tb) {
    
    def kin(x: ta.Kind): Result[tb.Kind] = x
    
    
    def nameHint(x: ta.Term): ?[Sym] = None
    
    def mkBlock(stmts: b.Stmt*)(ret: tb.Node) = tb.Node(ret.term match {
      case tb.Block(s, r) => tb.Block(stmts ++ s toList, r)
      case _ => tb.Block(stmts toList, ret)
    }, ret.md) // TODO: Stmt should have a node, not be a Value!.. // MixedOrg((stmts map (_ org)) :+ ret.org))
    
  }
  
  
}


/*

class ToANF extends StageConverter[AST.type, ANF.type](AST, ANF) {
  import b._
  import b.values._
  
  val phaseName = "ToANF"
  
  type Result[T] = Monad.Simple[T]
  val Result = Monad.Simple
  
////  def vnod(x: a.ValueNode)(implicit c: Ctx): b.ValueNode = b.ValueNode(processVal(x.term), x.org)
////  def tnod(x: a.TypeNode)(implicit c: Ctx): b.TypeNode = b.TypeNode(processTyp(x.term), x.org)
//  def vnod(x: a.ValueNode)(implicit c: Ctx): b.ValueNode = b.Node(processVal(x.term), x.org)
//  def tnod(x: a.TypeNode)(implicit c: Ctx): b.TypeNode = b.Node(processTyp(x.term), x.org)
  def vnod[T <: av.LetTerm](x: av.Node[T])(implicit c: Ctx) = Node(processLetTermV(x.term), x.md)
  def tnod[T <: at.LetTerm](x: at.Node[T])(implicit c: Ctx) = t.Node(processLetTermT(x.term), x.md)
  
//  def processNode[T](x: a.Node[T])(implicit c: Ctx) = x match {
//    case x @ a.Node(_: a.Value, _) => vnod(x)
//  }
  
  def processLetTermV(x: av.LetTerm)(implicit c: Ctx): Result[LetTerm] = x match {
    case _ => ???
  }
  def processLetTermT(x: at.LetTerm)(implicit c: Ctx): Result[t.LetTerm] = x match {
    case _ => ???
  }
  def processVal(x: a.Value)(implicit c: Ctx): Result[Value] = x match {
    case av.Lambda(id: Ident, bo) => Closure(id, vnod(bo))
    case av.Lambda(pa, bo) =>
      val id = new SyntheticId(nameHint(pa.term))
      val idn = Node(Id(id): LetTerm, Synthetic(phaseName, pa.org))
      val let = Let(Modification(false), vnod(pa), idn)
      Closure(id, mkBlock(let)(vnod(bo)))
  }
  
  def processTyp(x: a.Type)(implicit c: Ctx): Result[Type] = ???
  
  
  def nameHint(x: a.Value): ?[Sym] = None
  
  def mkBlock(stmts: Stmt*)(ret: ValueNode) = Node(ret.term match {
    case Block(s, r) => Block(stmts ++ s, r)
    case _ => Block(stmts, ret)
  }, ret.org) // TODO: Stmt should have a node, not be a Value!.. // MixedOrg((stmts map (_ org)) :+ ret.org))
  
}
*/













