package front2

import utils._
import common._
//import Stages._
import scala.util.{ Try, Success, Failure }
import collection._

abstract class Converter {
  
  type Ctx
  type Result[T]
  implicit val Result: Monad[Result]
  
}
/*

//abstract case class TermsConverter[A <: Stage2# TermsTemplate, B <: Stage2# TermsTemplate](a: A, b: B) extends Converter{
abstract case class TermsConverter[SA <: Stage2, SB <: Stage2, A <: SA# TermsTemplate, B <: SB# TermsTemplate](sa: SA, sb: SB, a: A, b: B) extends Converter{
  import Result._
  
  def nod(x: a.Node): Result[b.Node]
  def snod(x: a.SubNode): Result[b.SubNode]
  
  def process(x: a.GenTerm): Result[b.GenTerm] = x match {
    case a.Literal(v) => b.Literal(v) |> lift
    case a.Id(id) => b.Id(id) |> lift
    case a.Atom(na,ar) => Monad.sequence(ar map snod) map { b.Atom(na, _) }
    case a.App(fu,ar) => for(fu <- snod(fu); ar <- snod(ar)) yield b.App(fu,ar)
    case x: a.CoreTerm => process(x)
  }
  def process(x: a.CoreTerm): Result[b.CoreTerm] = x match {
    case a.Closure(pa, bo) => //for (pa <- process(pa); bo <- process(pa))
      nod(bo) map (b.Closure(pa, _))
  }
  /*
  def process(x: a.ComStmt): Result[b.ComStmt] = x match {
    case a.Let(mo,pa,bo,wh) => ??? //for(pa <- snod(pa); bo <- nod(bo)) yield b.Let(pa,bo)
    //case sa.values.Impure(n) => nod(n)
  }
  */
}
*/

abstract case class StageConverter[A <: Stage2, B <: Stage2](a: A, b: B) extends Converter {
conv =>
  import Result._
  
  def tstmt(x: a.TypeStmt): Result[b.TypeStmt]
  def vstmt(x: a.ValueStmt): Result[b.ValueStmt]
  def mod(x: a.Modif): Result[b.Modif]
  
  val tconv: TermsConverter[a.types.type,b.types.type]
  val vconv: TermsConverter[a.values.type,b.values.type] //(a.values,b.values)
  
  
  def process(x: a.Stmt): Result[b.Stmt] = x match {
    case Left(ts) => tstmt(ts) map ((s:b.TypeStmt) => Left(s))
    case Right(vs) => vstmt(vs) map ((s:b.ValueStmt) => Right(s))
  }
  
  
  abstract case class TermsConverter[TA <: a.TermsTemplate, TB <: b.TermsTemplate](ta: TA, tb: TB) {
    import Result._
    
    val co: TermsConverter[ta.dualWorld.type, tb.dualWorld.type]
    
    def nod(x: ta.Node): Result[tb.Node]
    def snod(x: ta.SubNode): Result[tb.SubNode]
    def kin(x: ta.Kind): Result[tb.Kind]
    
    def process(x: ta.SubTerm): Result[tb.SubTerm] = x match {
      // ComTerm
      case ta.Literal(v) => tb.Literal(v) |> lift
      case ta.Id(id) => tb.Id(id) |> lift
      case ta.Atom(na,ar) => Monad.sequence(ar map snod) map { tb.Atom(na, _) }
      case ta.Ascribe(v, k) => for (v <- snod(v); k <- kin(k)) yield tb.Ascribe(v, k)
    }
    def process(x: ta.GenTerm): Result[tb.GenTerm] = x match {
      case ta.App(fu,ar) => for(fu <- snod(fu); ar <- snod(ar)) yield tb.App(fu,ar)
      case ta.DepApp(fu,ar) => for(fu <- snod(fu); ar <- co.snod(ar)) yield tb.DepApp(fu,ar)
      case ta.Block(sts, re) => for (sts <- Monad.sequence(sts map conv.process); re <- nod(re)) yield tb.Block(sts, re)
      // SubTerm
      case x: ta.SubTerm => process(x)
      // AstTerm
      case x: ta.ASTTerm => process(x)
      // CoreTerm
      case x: ta.CoreTerm => process(x)
    }
    def processA(x: ta.ASTTerm): Result[tb.ASTTerm] = x match {
      case ta.Lambda(pa, bo) => for(pa <- nod(pa); bo <- nod(bo)) yield tb.Lambda(pa,bo)
      case ta.OpApp(ar, op) => snod(ar) map {tb.OpApp(_, op)}
    }
    def process(x: ta.CoreTerm): Result[tb.CoreTerm] = x match {
      case ta.Closure(pa, bo) => //for (pa <- process(pa); bo <- process(pa))
        nod(bo) map (tb.Closure(pa, _))
    }
    
    def process(x: ta.ComStmt): Result[tb.ComStmt] = x match {
      case ta.Let(mo,pa,bo,wh) => for {
        mo <- mod(mo)
        pa <- nod(pa)
        bo <- nod(bo)
        wh <- Monad.sequence(wh map conv.process)
      } yield tb.Let(mo,pa,bo,wh)
      //case a.values.Impure(n) => println(n); ??? //nod(n)
//      case a.Impure(n) => println(n); ??? //nod(n)
      //case im: ta.Impure => println(im); ??? //nod(n)
//      case im: A# Impure => nod(im.n.asInstanceOf[ta.Node]) map (b.Impure(_))
      case Impure(n) => nod(n) map (Impure(_))
    }
//    //type Imp <: a.TermsTemplate# ComStmt = a.values.Impure
//    def process(x: ta.ComStmt): Result[tb.ComStmt] =
//      //ta match {
//      (ta: a.TermsTemplate) match {
//        case a.values =>
//          x match {
//            case ta.Let(mo,pa,bo,wh) => ??? //for(pa <- snod(pa); bo <- nod(bo)) yield b.Let(pa,bo)
//            //case a.values.Impure(n) => println(n); ??? //nod(n)
////            case a.Impure(n) => println(n); ???
////            //case im: ta.Impure => println(im); ??? //nod(n)
//            case im: A# Impure => println(im); ??? //nod(n)
////            case x: Imp => println(x); ???
//          }
//      }
    
    def processC(x: ta.ASTStmt): Result[tb.ASTStmt] = x match {
      case ta.ModBlock(mo, sts) => for(mo <- mod(mo); sts <- Monad.sequence(sts map conv.process)) yield tb.ModBlock(mo, sts)
    }
    def process(x: ta.CoreStmt): Result[tb.CoreStmt] = x match {
      case ta.RecBlock(sts) => for(sts <- Monad.sequence(sts map conv.process)) yield tb.RecBlock(sts)
    }
  
    /**
     * Scala fails to understand that {{{a.Impure <: a.values.ComStmt}}}, so we provide ctor/xtor to handle that case
     */
    object Impure {
      def apply(n: tb.Node) = b.Impure(n.asInstanceOf[b.values.Node]).asInstanceOf[tb.ComStmt]
      def unapply(im: A# Impure) = Some(im.n.asInstanceOf[ta.Node])
    }
    
  }
}


/*

abstract case class StageConverter[A <: Stage2, B <: Stage2](a: A, b: B) extends Converter {
  import b._
  import b.values._

  val t = b.types
  
  val at = a.types
  val av = a.values
  val bt = b.types
  val bv = b.values
  
  
  //  def apply(defs: Seq[a.Decl])(implicit c: Ctx): Seq[Decl] = defs map process
  
  /** Polymorphic definitions */
  
  //def md(x: a.Metadata): b.Metadata
  
  //  def typs(x: a.TypSym)(implicit c: Ctx): b.TypSym
  //  def vals(x: a.ValSym)(implicit c: Ctx): b.ValSym
//  def vnod(x: a.ValueNode)(implicit c: Ctx): Result[b.ValueNode]
//  def tnod(x: a.TypeNode)(implicit c: Ctx): Result[b.TypeNode]
  def vnod(x: av.Node)(implicit c: Ctx): Result[bv.Node]
  def tnod(x: av.Node)(implicit c: Ctx): Result[bv.Node]
//  def vnod[T <: av.LetTerm](x: av.Node[T])(implicit c: Ctx): Result[Node[LetTerm]]
//  def tnod[T <: at.LetTerm](x: at.Node[T])(implicit c: Ctx): Result[t.Node[t.LetTerm]]
  
  //def processNode[T](x: a.Node[T])(implicit c: Ctx): Result[b.Node[T]] //= x match { }
  
  def processComTerm(tta: a.TermsTemplate, ttb: TermsTemplate)
                 (x: tta.ComTerm)//, nods: tta.Node => Result[ttb.Node])
                 (implicit c: Ctx): Result[ttb.ComTerm] =
  { import tta._
    x match {
      case Literal(v) => ttb.Literal(v) |> lift
//      case App(f, a) => //ttb.App(nods(f), nods(a))
//        for (f <- nods(f); a <- nods(a)) yield ttb.App(f, a)
//        ???
    }
  }
  
//  def processVal(x: a.Value)(implicit c: Ctx): Value = (x/*: @unchecked*/) match {
//    case x: ComTerm => processComTerm(a.values, b.values)(x, vnods)
//    case a.Ascribe(v, t) => Ascribe(vnods(v), tnods(t))
//  }
  
  def processVal(x: a.Value)(implicit c: Ctx): Result[Value]
  
  def processTyp(x: a.Type)(implicit c: Ctx): Result[Type]
  
  
  
  
}

abstract class StageTraverser[A <: Stage2](a: A) extends StageConverter[A,A](a,a) {
  import a._
  
  type Result[T] = Monad.Discard[T]
  val Result = Monad.UnitMonad
  
//  def vnods(x: a.ValueNode)(implicit c: Ctx): Result[b.ValueNode]
//  def tnods(x: a.TypeNode)(implicit c: Ctx): Result[b.TypeNode]
  
  
}
*/











