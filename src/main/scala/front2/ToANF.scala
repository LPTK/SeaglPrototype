package front2

import utils._

import common._
import Stages2._


class ToANF extends StageConverter[AST.type, ANF.type](AST, ANF) {
toanf =>
  
  val phaseName = "ToANF"
  
//  type Result[T] = Ls[T]
//  val Result = Monad.ListMonad
  /**
   * Stores the transformed object (in `res`) as well as the new statements that the transformation has generated (in `genStmts`)
   * TODO: define a State monad?
   */
  case class Result[T](genStmts: Ls[b.Stmt], res: T) {
    //def flatten = genStmts :+ res
  }
  implicit object Result extends Monad[Result] {
    def lift[A](a: A) = Result(Nil, a)
    def flatMap[A,B](ma: M[A], f: A => M[B]): M[B] = {
      val Result(newStmts, r) = f(ma.res)
      Result(ma.genStmts ++ newStmts, r)
    }
    def map[A,B](ma: F[A], f: A => B): F[B] = Result(ma.genStmts, f(ma.res))
  }
  import Result._
  
//  //def tstmt(x: a.TypeStmt): Result[b.TypeStmt] = tconv.process(x)
//  //def tstmt(x: a.TypeStmt): Result[ANF.types.CoreStmt] = tconv.process(x: AST.types.ASTStmt)//: Result[ANF.types.CoreStmt]
//  //def tstmt(x: a.TypeStmt): Result[ANF.types.CoreStmt] = tconv.processC(x: tconv.ta.ASTStmt): Result[ANF.types.CoreStmt]
//  def tstmt(x: a.TypeStmt): Result[b.TypeStmt] = x match {
//    case a.types.ModBlock(mo, sts) =>
//      // TODO check mo; make type or value + use Priv
//      // TODO what to do when NOT creating a RecBlock??
//      val sts2 = sts map process flatMap { case Result(s,r) => s :+ r } //map (_ flatten)
//      if (mo contains Rec) b.types.RecBlock(sts2) |> lift //b.values.RecBlock(Monad.sequence(sts map process))
//      else Result(sts2.init, sts2.last)
//  }
  def tstmt(x: a.TypeStmt): Result[b.TypeStmt] = wtf
  def vstmt(x: a.ValueStmt): Result[b.ValueStmt] = wtf
//  def tstmt(x: a.TypeStmt): Result[b.TypeStmt] = tconv.stmt(x)
//  def vstmt(x: a.ValueStmt): Result[b.ValueStmt] = ??? //vconv.stmt(x)
  
  /* // TYPES:
  override def process(x: a.Stmt): Result[b.Stmt] = x match {
    case Left(ts) => ts match {
      case a.types.ModBlock(mo, sts) =>
      // TODO check mo; make type or value + use Priv
      // TODO what to do when NOT creating a RecBlock??
      val sts2 = sts map process flatMap { case Result(s,r) => s :+ r }
      //if (mo contains Rec) lift(b.types.RecBlock(sts2)) //|> lift
      if (mo contains Rec) b.types.RecBlock(sts2) |> b.t2stmt |> lift
      else Result(sts2.init, sts2.last)
    }
    case Right(vs) => ???
  }
  */
  /** Expects that in AST, ModBlocks have the modifier Type if it contains type statements */
  override def process(x: a.Stmt): Result[b.Stmt] = {
    //val stmt = x.fold(_:a.types.ASTStmt,_:a.values.ASTStmt)
    val stmt = x.fold(identity,identity)
    stmt match {
//      case a.types.ModBlock(mo, sts) =>
//        val sts2 = sts map process flatMap { case Result(s,r) => s :+ r }
//        if (mo contains Rec) b.types.RecBlock(sts2) |> b.t2stmt |> lift
//        else Result(sts2.init, sts2.last)
//      case a.values.ModBlock(mo, sts) =>
//        val sts2 = sts map process flatMap { case Result(s,r) => s :+ r }
//        if (mo contains Rec) b.values.RecBlock(sts2) |> b.v2stmt |> lift
//        else Result(sts2.init, sts2.last)
      //case mb: a.TermsTemplate# ModBlock =>
      case a.ModBlock(modifs, stmts) =>
        val sts2 = stmts map process flatMap { case Result(s,r) => s :+ r }
        if (modifs contains Rec)
          if (modifs contains Type) b.types.RecBlock(sts2) |> b.t2stmt |> lift
          else b.values.RecBlock(sts2) |> b.v2stmt |> lift
        else Result(sts2.init, sts2.last)
      case x: a.types.ComStmt => tconv.process(x) map Left.apply
      case x: a.values.ComStmt => vconv.process(x) map Right.apply
    }
  }
  
  def mod(x: a.Modif): Result[b.Modif] = Modification(x contains Priv) |> lift
  
  //val tconv: TermsConverter[AST.types.type,ANF.types.type] = new AnfTermsConverter[AST.types.type,ANF.types.type](AST.types, ANF.types) {
  val tconv: AnfTermsConverter[AST.types.type,ANF.types.type] = new AnfTermsConverter[AST.types.type,ANF.types.type](AST.types, ANF.types) {
    val co: vconv.type = vconv
    
    def blockAsTerm = identity
    
    def kin(x: ta.Kind): Result[tb.Kind] = x |> lift
    
    /** For types, we know nodes will never generate additional statements (types.App is not moved out of subexpressions) */
    def nod(x: ta.Node): Result[tb.Node] = tb.Node(x.term match { //tb.Node(processA(x.term), x.md)
      //case ta.Lambda(pa, bo) => ???
      case ta.Lambda(id: Ident, bo) => tb.Closure(id, nod(bo).res)
      case ta.Lambda(pa, bo) =>
        val id = new SyntheticId(nameHint(pa.term))
        val idn = tb.Node(tb.Id(id): tb.Term, Synthetic(phaseName, pa.md))
        val let = tb.Let(Modification(false), nod(pa).res, idn)
        tb.Closure(id, mkBlock(let)(nod(bo).res))
      case ta.OpApp(ar, op) => tb.App(tb.Node(tb.Id(op.id), x.md/*TODO better md*/), nod(ar).res)
    }, x.md) |> lift
    
//    def snod(x: ta.SubNode): Result[tb.SubNode] = new b.ValueSubNode(x.term match { // TODO: how to properly convert a nested App?
//      case _ => ???
//    }, x.md)
    def snod(x: ta.SubNode): Result[tb.SubNode] = tb.Node(process(x.term).res, x.md) |> lift
    
  }
  //val vconv: AnfTermsConverter[AST.values.type,ANF.values.type] = ???
  val vconv/*: TermsConverter[a.values.type,b.values.type]*/ = new AnfTermsConverter[AST.values.type,ANF.values.type](AST.values, ANF.values) {
    val co: tconv.type = tconv
    
    def blockAsTerm = identity// (bl: tb.Block) => bl //
    
    def kin(x: ta.Kind): Result[tb.Kind] = tconv.process(x)
    
    /** For values, a simple node may generate several new statements */
//    def nod(x: ta.Node): Result[tb.Node] = ??? 
//      tb.Node(x.term match { //tb.Node(processA(x.term), x.md)
//      //case ta.Lambda(pa, bo) => ???
//      case ta.Lambda(id: Ident, bo) => tb.Closure(id, nod(bo).head)
//      case ta.Lambda(pa, bo) =>
//        val id = new SyntheticId(nameHint(pa.term))
//        val idn = tb.Node(tb.Id(id): tb.Term, Synthetic(phaseName, pa.md))
//        val let = tb.Let(Modification(false), nod(pa).head, idn)
//        tb.Closure(id, mkBlock(let)(nod(bo).head))
//      case ta.OpApp(ar, op) => tb.App(tb.Node(tb.Id(op.id), x.md/*TODO better md*/), nod(ar).head)
//    }, x.md) :: Nil
    
//    def snod(x: ta.SubNode): Result[tb.SubNode] = {
    def nod(x: ta.Node): Result[tb.Node] = process(x.term) map {tb.Node(_, x.md)}
    def snod(x: ta.SubNode): Result[tb.SubNode] = {
      val Result(sts, ret) = x.term match {
        case ta.Lambda(id: Ident, bo) => nod(bo) map {tb.Closure(id, _)}
        case ta.Lambda(pa, bo) =>
          val id = new SyntheticId(nameHint(pa.term))
          val idn = tb.Node(tb.Id(id): tb.Term, Synthetic(phaseName, pa.md))
          for {
            pa <- nod(pa)
            let = tb.Let(Modification(false), pa, idn)
            bo <- nod(bo)
          } yield tb.Closure(id, mkBlock(let)(bo))
        case st: ta.SubTerm => process(st)
        case gt: ta.GenTerm => process(gt) flatMap { gt =>
          val id = new SyntheticId(nameHint(x.term))
          val idt = tb.Id(id)//: tb.SubTerm
          val idn = tb.Node(idt, Synthetic(phaseName, x.md))
          val let = tb.Let(Modification(false), idn, tb.Node(gt, x.md))
          Result(let :: Nil, idt)
        }
      }
      Result(sts, new b.ValueSubNode(ret, x.md))
    }
    
//    def snod(x: ta.SubNode): Result[tb.SubNode] = tb.Node(process(x.term).head, x.md) :: Nil
  }
  
  abstract class AnfTermsConverter[
    TA <: a.TermsTemplate,
    //TB <: b.TermsTemplate { type Term = TB# GenTerm; type Kind = TA# Kind }
    TB <: b.TermsTemplate //{ type Kind = TA# Kind }
  ](val _ta: TA, val _tb: TB) extends TermsConverter[TA,TB](_ta,_tb) {
    //import Result._
    
    //def blockAsTerm(bl: tb.Block): tb.Term
    def blockAsTerm: tb.Block => tb.Term
    
    //def kin(x: ta.Kind): Result[tb.Kind] = x |> lift
    
//    def stmt(x: ta.ASTStmt): Result[tb.CoreStmt] = x match {
//      case ta.ModBlock(mo, sts) => 
//        val sts2 = sts map toanf.process flatMap { case Result(s,r) => s :+ r }
//        if (mo contains Rec) tb.RecBlock(sts2) |> lift
//        else Result(sts2.init, sts2.last) // sts2.last could be of wrong type
//    }
//    def lambda(lam: ta.Lambda) = {
//      val ta.Lambda(pa, bo) = lam
//      val id = new SyntheticId(nameHint(pa.term))
//      val idn = tb.Node(tb.Id(id): tb.Term, Synthetic(phaseName, pa.md))
//      val let = tb.Let(Modification(false), nod(pa).res, idn)
//      tb.Closure(id, mkBlock(let)(nod(bo).res))
//    }
    
    def nameHint(x: ta.Term): ?[Sym] = None
    
    def mkBlock(stmts: b.Stmt*)(ret: tb.Node) = tb.Node(ret.term match {
      case tb.Block(s, r) => tb.Block(stmts ++ s toList, r) |> blockAsTerm
      case _ => tb.Block(stmts toList, ret) |> blockAsTerm
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













