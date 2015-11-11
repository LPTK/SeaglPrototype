package front2

import utils._
import common._
import scala.util.{ Try, Success, Failure }
import collection._

/** TODO rename process -> apply */
abstract case class StageConverter[A <: Stage2, B <: Stage2](a: A, b: B) extends Converter {
conv =>
  import Result._
  
  /** Polymorphic Transformations */
  
//  def tstmt(x: a.TypeStmt): Result[b.TypeStmt]
//  def vstmt(x: a.ValueStmt): Result[b.ValueStmt]
  def mod(x: a.Modif): Result[b.Modif]
  
  val tconv: TermsConverter[a.types.type,b.types.type]
  val vconv: TermsConverter[a.values.type,b.values.type]
  
  
  /** Default Transformations */
  
//  def process(x: a.AnyStmt): Result[b.AnyStmt] = x match {
//    case Left(ts) => tstmt(ts) map ((s:b.TypeStmt) => Left(s))
//    case Right(vs) => vstmt(vs) map ((s:b.ValueStmt) => Right(s))
//  }
  def process(x: a.AnyStmt): Result[b.AnyStmt] = x match {
    case Left(ts) => tconv.stmt(ts) map (s => Left(s))
    case Right(vs) => vconv.stmt(vs) map (s => Right(s))
  }
  
  abstract case class TermsConverterClass[+TA <: a.TermsTemplate, +TB <: b.TermsTemplate](ta: TA, tb: TB) extends TermsConverter[TA,TB] {
  }
  
  trait TypeConverter {
    val self = Left(tconv): tconv.type | vconv.type
  }
  trait ValueConverter {
    val self = Right(vconv): tconv.type | vconv.type
  }
  
  // TODO: define ValuesConverter and TypesConverter with `co` already set?
  trait TermsConverter[+TA <: a.TermsTemplate, +TB <: b.TermsTemplate] {
    import Result._
    
    val ta: TA
    val tb: TB
    
    
    /** Dual Terms Converter */
    
    //val co: TermsConverter[ta.dualWorld.type, tb.dualWorld.type]
    ////val co: TermsConverter[ta.DualWorld, tb.DualWorld]
    /** Should return Left(this) or Right(this);  (tconv.type with this.type) does not work */
    //val lol: (this.type =:= tconv.type) | (this.type =:= vconv.type)
    val self: tconv.type | vconv.type //= ???
    lazy val co: TermsConverter[ta.dualWorld.type, tb.dualWorld.type] =
      //(self match { case Left(`tconv`) => vconv  case Right(`vconv`) => tconv })
      (self match { case Left(_) => vconv  case Right(_) => tconv })
      .asInstanceOf[TermsConverter[ta.dualWorld.type, tb.dualWorld.type]]
    
    
    /** Polymorphic definitions */
  
    ///** Node could be implemented here, but then we'd have to provide a function for Metadata */
    def nod(x: ta.Node): Result[tb.Node]
    def snod(x: ta.SubNode): Result[tb.SubNode]
    def kin(x: ta.Kind): Result[tb.Kind]
    //def mod(x: ta.Modif): Result[tb.Modif]
    def stmt(x: ta.Stmt): Result[tb.Stmt]
    
    
    /** Default Transformations */
  
    def process(x: ta.ComTerm): Result[tb.ComTerm] = x match {
      // SubTerm
      case ta.Literal(v) => tb.Literal(v) |> lift
      case ta.Id(id) => tb.Id(id) |> lift
      case ta.Atom(na,ar) => Monad.sequence(ar map snod) map { tb.Atom(na, _) }
      case ta.Ascribe(v, k) => for (v <- snod(v); k <- kin(k)) yield tb.Ascribe(v, k)
      // Other terms
      case ta.App(fu,ar) => for(fu <- snod(fu); ar <- snod(ar)) yield tb.App(fu,ar)
      case ta.DepApp(fu,ar) => for(fu <- snod(fu); ar <- co.snod(ar)) yield tb.DepApp(fu,ar)
      case ta.Block(sts, re) => for (sts <- Monad.sequence(sts map conv.process); re <- nod(re)) yield tb.Block(sts, re)
      // SubTerm
      //case x: ta.SubTerm => process(x)
      // AstTerm
//      case x: ta.ASTTerm => process(x)
//      // CoreTerm
//      case x: ta.CoreTerm => process(x)
        case _ => scalasDumb(x)
    }
    def process(x: ta.SubTerm): Result[tb.SubTerm] = x match {
      //case x: ta.ASTTerm => process(x) // Note: for some reason, putting this case (which cannot happen) makes scalac think the rest is dead code!!
      case x: ta.ComTerm => process(x)
      case _ => scalasDumb(x)
    }
    def process(x: ta.ASTTerm): Result[tb.ASTTerm] = x match {
      case ta.Lambda(pa, bo) => for(pa <- nod(pa); bo <- nod(bo)) yield tb.Lambda(pa,bo)
      case ta.OpAppL(ar, op) => snod(ar) map {tb.OpAppL(_, op)}
      case ta.OpAppR(op, ar) => snod(ar) map {tb.OpAppR(op, _)}
      case ta.OpTerm(op) => tb.OpTerm(op) |> lift
      case let: ta.Let => process(let)
      case x: ta.ComTerm => process(x)
      case _ => scalasDumb(x)
    }
    def process(x: ta.CoreTerm): Result[tb.CoreTerm] = x match {
      case ta.Closure(pa, bo) => //for (pa <- process(pa); bo <- process(pa))
        nod(bo) map (tb.Closure(pa, _))
      case x: ta.ComTerm => process(x)
      case _ => scalasDumb(x)
    }
    
    def process(x: ta.Let): Result[tb.Let] = x match {
      case ta.Let(mo,pa,bo,wh) => for {
        mo <- mod(mo)
        pa <- nod(pa)
        bo <- nod(bo)
        wh <- Monad.sequence(wh map conv.process)
      } yield tb.Let(mo,pa,bo,wh)
    }
    
    def process(x: ta.ComStmt): Result[tb.ComStmt] = x match {
      case x: ta.Let => process(x)
      //case Impure(n) => nod(n) map (Impure(_))
      case ta.Impure(n) => nod(n) map (tb.Impure(_))
      case _ => scalasDumb(x)
    }
    def process(x: ta.ASTStmt): Result[tb.ASTStmt] = x match {
      case ta.ModBlock(mo, sts) => for(mo <- mod(mo); sts <- Monad.sequence(sts map conv.process)) yield tb.ModBlock(mo, sts)
      case x: ta.ComStmt => process(x)
      case _ => scalasDumb(x)
    }
    def process(x: ta.CoreStmt): Result[tb.CoreStmt] = x match {
      case ta.RecBlock(sts) => for(sts <- Monad.sequence(sts map conv.process)) yield tb.RecBlock(sts)
      case x: ta.ComStmt => process(x)
      case _ => scalasDumb(x)
    }
  
    /*
    /**
     * Scala fails to understand that {{{a.Impure <: a.values.ComStmt}}}, so we provide ctor/xtor to handle that case
     */
    object Impure {
      def apply(n: tb.Node) = b.Impure(n.asInstanceOf[b.values.Node]).asInstanceOf[tb.ComStmt]
      //def unapply(im: A# Impure) = Some(im.n.asInstanceOf[ta.Node])
      def unapply(im: a.TermsTemplate# ComStmt) = im match { //Some(im.n.asInstanceOf[ta.Node])
        case a.Impure(n) => n.asInstanceOf[ta.Node] |> Some.apply
        case _ => None
      }
    }
    */
    
  }
}


abstract class SameStageConverterClass[A <: Stage2](a: A) extends StageConverter[A,A](a,a) {
//abstract class SameStageConverter[A <: Stage2](val _a: A) extends StageConverter[A,A](_a,_a) {
//abstract class SameStageConverter[A <: Stage2](override val a: A) extends StageConverter[A,A](a,a) {
//  override val b: a.type = a
//  def mod(x: a.Modif) = (x:b.Modif) |> Result.lift
}
//trait SameStageConverter[A <: Stage2] extends StageConverter[A,A] {
//  def mod(x: a.Modif) = (x:b.Modif) |> Result.lift
//}








