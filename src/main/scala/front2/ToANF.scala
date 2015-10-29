package front2

import utils._

import common._
import Stages2._

class ToANF extends StageConverter[AST.type, ANF.type](AST, ANF) {
  import b._
  import b.values._
  
  val phaseName = "ToANF"
  
  type Result[T] = Monad.Simple[T]
  val Result = Monad.Simple
  
  def vnod(x: a.ValueNode)(implicit c: Ctx): b.ValueNode = b.ValueNode(processVal(x.term), x.org)
  def tnod(x: a.TypeNode)(implicit c: Ctx): b.TypeNode = b.TypeNode(processTyp(x.term), x.org)
  
  
  def processVal(x: a.Value)(implicit c: Ctx): Result[Value] = x match {
    case av.Lambda(id: Ident, bo) => Closure(id, vnod(bo))
    case av.Lambda(pa, bo) =>
      val id = new SyntheticId(nameHint(pa.term))
      val idn = ValueNode(Id(id), Synthetic(phaseName, pa.org))
      Closure(id, mkBlock(Let(Ls(), pa, idn))(vnod(bo)))
  }
  
  def processTyp(x: a.Type)(implicit c: Ctx): Result[Type] = ???
  
  
  def nameHint(x: a.Value): ?[Sym] = None
  
  def mkBlock(stmts: Stmt*)(ret: ValueNode) = ValueNode(ret.term match {
    case Block(s, r) => Block(stmts ++ s, r)
    case _ => Block(stmts, ret)
  }, ret.org) // TODO: Stmt should have a node, not be a Value!.. // MixedOrg((stmts map (_ org)) :+ ret.org))
  
}













