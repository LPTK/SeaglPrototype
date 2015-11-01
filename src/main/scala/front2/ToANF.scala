package front2

import utils._

import common._
import Stages2._


object ToANF extends StageConverter[AST.type, ANF.type](AST, ANF) {
toanf =>
  
  val phaseName = "ToANF"
  
  /**
   * Stores the transformed object (in `res`) as well as the new statements that the transformation has generated (in `genStmts`)
   * Note: could be done with a State monad?
   */
  case class Result[T](genStmts: Ls[b.AnyStmt], res: T)
  implicit object Result extends Monad[Result] {
    def lift[A](a: A) = Result(Nil, a)
    def flatMap[A,B](ma: M[A], f: A => M[B]): M[B] = {
      val Result(newStmts, r) = f(ma.res)
      Result(ma.genStmts ++ newStmts, r)
    }
    def map[A,B](ma: F[A], f: A => B): F[B] = Result(ma.genStmts, f(ma.res))
  }
  import Result._
  
  /** `tstmt` and `vstmt` are never supposed to be called -- `process` will deal with statements directly */
  def tstmt(x: a.TypeStmt): Result[b.TypeStmt] = wtf
  def vstmt(x: a.ValueStmt): Result[b.ValueStmt] = wtf
  
  /** Expects that in AST, ModBlocks have the modifier Type if it contains type statements */
  override def process(x: a.AnyStmt): Result[b.AnyStmt] = {
    val stmt = x.fold (identity, identity)
    stmt match {
      case a.ModBlock(modifs, stmts) =>
        val sts2 = stmts map process flatMap { case Result(s,r) => s :+ r }
        if (modifs contains Rec)
          if (modifs contains Type) b.types.RecBlock(sts2) |> b.t2stmt |> lift
          else b.values.RecBlock(sts2) |> b.v2stmt |> lift
        else Result(sts2.init, sts2.last)
      case x: a.types.Stmt => tconv.process(x) map Left.apply
      case x: a.values.Stmt => vconv.process(x) map Right.apply
    }
  }
  
  def mod(x: a.Modif): Result[b.Modif] = Modification(x contains Priv) |> lift
  
  val tconv: AnfTermsConverter[AST.types.type,ANF.types.type] = new AnfTermsConverter(AST.types, ANF.types) {
    val co: vconv.type = vconv
    
    def blockAsTerm = identity
    
    def kin(x: ta.Kind): Result[tb.Kind] = x |> lift
    
    /** For types, we know nodes will never generate additional statements (types.App is not moved out of subexpressions) */
    def nod(x: ta.Node): Result[tb.Node] = tb.Node(x.term match {
      case ta.Lambda(id: Ident, bo) => tb.Closure(id, nod(bo).res)
      case ta.Lambda(pa, bo) =>
        val id = new SyntheticId(nameHint(pa.term))
        val idn = tb.Node(tb.Id(id): tb.Term, Synthetic(phaseName, pa.md))
        val let = tb.Let(Modification(false), nod(pa).res, idn)
        tb.Closure(id, mkBlock(let)(nod(bo).res))
      case ta.OpApp(ar, op) => tb.App(tb.Node(tb.Id(op.id), x.md/*TODO better md*/), nod(ar).res)
    }, x.md) |> lift
    
    def snod(x: ta.SubNode): Result[tb.SubNode] = tb.Node(process(x.term).res, x.md) |> lift
    
  }
  val vconv = new AnfTermsConverter(AST.values, ANF.values) {
    val co: tconv.type = tconv
    
    def blockAsTerm = identity
    
    def kin(x: ta.Kind): Result[tb.Kind] = tconv.process(x)
    
    def nod(x: ta.Node): Result[tb.Node] = process(x.term) map {tb.Node(_, x.md)}
    
    /** For values, a simple node may generate several new statements */
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
    
  }
  
  abstract class AnfTermsConverter[TA <: a.TermsTemplate, TB <: b.TermsTemplate](val _ta: TA, val _tb: TB)
  extends TermsConverter[TA,TB](_ta,_tb) {
    
    def blockAsTerm: tb.Block => tb.Term
    
    def nameHint(x: ta.Term): ?[Sym] = None
    
    def mkBlock(stmts: b.AnyStmt*)(ret: tb.Node) = tb.Node(ret.term match {
      case tb.Block(s, r) => tb.Block(stmts ++ s toList, r) |> blockAsTerm
      case _ => tb.Block(stmts toList, ret) |> blockAsTerm
    }, ret.md) // TODO: Stmt should have a node, not be a Value!.. // MixedOrg((stmts map (_ org)) :+ ret.org))
    
  }
  
}














