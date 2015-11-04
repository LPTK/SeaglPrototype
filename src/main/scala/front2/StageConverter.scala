package front2

import utils._
import common._
import scala.util.{ Try, Success, Failure }
import collection._

abstract case class StageConverter[A <: Stage2, B <: Stage2](a: A, b: B) extends Converter {
conv =>
  import Result._
  
  /** Polymorphic Transformations */
  
  def tstmt(x: a.TypeStmt): Result[b.TypeStmt]
  def vstmt(x: a.ValueStmt): Result[b.ValueStmt]
  def mod(x: a.Modif): Result[b.Modif]
  
  val tconv: TermsConverter[a.types.type,b.types.type]
  val vconv: TermsConverter[a.values.type,b.values.type]
  
  
  /** Default Transformations */
  
  def process(x: a.AnyStmt): Result[b.AnyStmt] = x match {
    case Left(ts) => tstmt(ts) map ((s:b.TypeStmt) => Left(s))
    case Right(vs) => vstmt(vs) map ((s:b.ValueStmt) => Right(s))
  }
  
  
  abstract case class TermsConverter[TA <: a.TermsTemplate, TB <: b.TermsTemplate](ta: TA, tb: TB) {
    import Result._
    
    
    /** Dual Terms Converter */
    
    val co: TermsConverter[ta.dualWorld.type, tb.dualWorld.type]
    
    
    /** Polymorphic definitions */
  
    /** Node could be implemented here, but then we'd have to provide a function for Metadata */
    def nod(x: ta.Node): Result[tb.Node]
    def snod(x: ta.SubNode): Result[tb.SubNode]
    def kin(x: ta.Kind): Result[tb.Kind]
    
    
    /** Default Transformations */
  
    def process(x: ta.GenTerm): Result[tb.GenTerm] = x match {
      case ta.App(fu,ar) => for(fu <- snod(fu); ar <- snod(ar)) yield tb.App(fu,ar)
      case ta.DepApp(fu,ar) => for(fu <- snod(fu); ar <- co.snod(ar)) yield tb.DepApp(fu,ar)
      case ta.Block(sts, re) => for (sts <- Monad.sequence(sts map conv.process); re <- nod(re)) yield tb.Block(sts, re)
      // SubTerm
      case x: ta.SubTerm => process(x)
      // AstTerm
//      case x: ta.ASTTerm => process(x)
//      // CoreTerm
//      case x: ta.CoreTerm => process(x)
    }
    def process(x: ta.SubTerm): Result[tb.SubTerm] = x match {
      case ta.Literal(v) => tb.Literal(v) |> lift
      case ta.Id(id) => tb.Id(id) |> lift
      case ta.Atom(na,ar) => Monad.sequence(ar map snod) map { tb.Atom(na, _) }
      case ta.Ascribe(v, k) => for (v <- snod(v); k <- kin(k)) yield tb.Ascribe(v, k)
    }
    def process(x: ta.ASTTerm): Result[tb.ASTTerm] = x match {
      case ta.Lambda(pa, bo) => for(pa <- nod(pa); bo <- nod(bo)) yield tb.Lambda(pa,bo)
      case ta.OpAppL(ar, op) => snod(ar) map {tb.OpAppL(_, op)}
      case ta.OpAppR(op, ar) => snod(ar) map {tb.OpAppR(op, _)}
      case ta.OpTerm(op) => tb.OpTerm(op) |> lift
      case let: ta.Let => ??? //process(let: ta.Stmt)
      case x: ta.GenTerm => process(x)
    }
    def process(x: ta.CoreTerm): Result[tb.CoreTerm] = x match {
      case ta.Closure(pa, bo) => //for (pa <- process(pa); bo <- process(pa))
        nod(bo) map (tb.Closure(pa, _))
      case x: ta.GenTerm => process(x)
    }
    
    def process(x: ta.Stmt): Result[tb.Stmt] = x match {
      case ta.Let(mo,pa,bo,wh) => for {
        mo <- mod(mo)
        pa <- nod(pa)
        bo <- nod(bo)
        wh <- Monad.sequence(wh map conv.process)
      } yield tb.Let(mo,pa,bo,wh)
      case Impure(n) => nod(n) map (Impure(_))
    }
    
    def process(x: ta.ASTStmt): Result[tb.ASTStmt] = x match {
      case ta.ModBlock(mo, sts) => for(mo <- mod(mo); sts <- Monad.sequence(sts map conv.process)) yield tb.ModBlock(mo, sts)
    }
    def process(x: ta.CoreStmt): Result[tb.CoreStmt] = x match {
      case ta.RecBlock(sts) => for(sts <- Monad.sequence(sts map conv.process)) yield tb.RecBlock(sts)
    }
  
    /**
     * Scala fails to understand that {{{a.Impure <: a.values.ComStmt}}}, so we provide ctor/xtor to handle that case
     */
    object Impure {
      def apply(n: tb.Node) = b.Impure(n.asInstanceOf[b.values.Node]).asInstanceOf[tb.Stmt]
      def unapply(im: A# Impure) = Some(im.n.asInstanceOf[ta.Node])
    }
    
  }
}


abstract class SameStageConverter[A <: Stage2](a: A) extends StageConverter[A,A](a,a)









