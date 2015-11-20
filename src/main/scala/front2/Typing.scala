package front2

import utils.Monad.State
import utils._

import common._
import Stages2._


object Typing extends StageConverter[Desugared.type, Typed.type](Desugared, Typed) {
conv =>
  
  //type Ctx = Sym ->? b.Type
  case class Ctx(typs: Ident ->? Types.TypeKind, vals: Ident ->? b.Type, inPattern: Bool = false)
  object Ctx { val empty = Ctx(->?.empty, ->?.empty) }
  
  type Result[+T] = Monad.State[Ctx, T]
  val Result = Monad.State[Ctx]
  import Result._
  
  implicit class Dummy[T](self: Result[T]) {
    def filter(p: T => Bool): Result[T] = {
      //ctx => { val (t, nctx) = self(ctx); require(p(t)); t -> nctx }
      self map { t => require(p(t)); t }
    }
  }
  
  //def modif(f: Ctx => Ctx): Result[Unit] = {
  //  case ctx => () -> f(ctx)
  //}
  def modif(f: Ctx => Ctx): Result[Unit] = f andThen (() -> _)
  
  def mod(x: a.Modif): Result[b.Modif] = x |> lift // FIXME
  
  
  def newTypVar(nameHint: Desugared.TermsTemplate# CoreTerm): b.Type = {
    b.types.Id(new SyntheticId(None)) // TODO use a nameHint
  }
  //def md(org: Origin, typ: b.Type): b.values.Metadata = ???
  
  //val vconv = ???
  object vconv extends TermsConverterClass[a.values.type, b.values.type](Desugared.values, Typed.values) with ValueConverter {
  
    def nod(x: vconv.ta.Node): State[Ctx, vconv.tb.Node] = //process(x.term) map {tb.Node(_, typeinfer(x.term))}
      for {
        (t, typ) <- typeinfer(x.term, x.md)
        //t <- process(x.term)
      } yield tb.Node(t, (x.md, typ))
    
    def snod(x: vconv.ta.SubNode): State[Ctx, vconv.tb.SubNode] =
      for {
        (t, typ) <- typeinferSub(x.term, x.md)
        //t <- subCoreTerm(x.term) //: ta.SubTerm)
      } yield tb.SubNode(t, (x.md, typ))
  
    def kin(x: Desugared.types.Node): State[Ctx, Typed.types.Node] = ???
    
    //def mod(x: ta.Modif): Result[tb.Modif]
    // ta.Stmt == Core.Stmt == Let|Impure|RecBlock
    //def stmt(x: ta.Stmt): State[Ctx, tb.Stmt] = x match {
    //  case ta.Let(mod, pa, bo, wh) => process(x)
    //  case _ => process(x)
    //}
    def stmt(x: ta.Stmt): State[Ctx, tb.Stmt] = process(x)
    
    override def process(x: ta.Let): Result[tb.Let] = x match {
      case ta.Let(mo,pa,bo,wh) => for {
        mo <- mod(mo)
        () <- modif(_.copy(inPattern = true))
        pa <- nod(pa)
        () <- modif(_.copy(inPattern = false))
        bo <- nod(bo)
        wh <- Monad.sequence(wh map conv.process)
      } yield tb.Let(mo,pa,bo,wh)
    }
    
    //
    
    def typeinfer(x: ta.Term, org: Origin): Result[tb.Term -> b.Type] = x match {
      case x: ta.SubTerm => typeinferSub(x, org)
        
      case ta.App(fun, arg) =>
        // TODO infer new typle class cstr for every typvar?
        for { fun <- snod(fun); arg <- snod(arg) }
          yield fun.term -> fun.md._2 // TODO //tb.App(fun, arg) -> b.types.App(fun.md._2, arg.md._2)
        
      case ta.DepApp(fu,ar) => ??? //for(fu <- snod(fu); ar <- co.snod(ar)) yield tb.DepApp(fu,ar)
        
      case ta.Block(sts, re) =>
        for (sts <- Monad.sequence(sts map conv.process); re <- nod(re))
          yield tb.Block(sts, re) -> re.md._2
        
    }
    
    //override def process(x: ta.ComTerm) = x match {
    def typeinferSub(x: ta.SubTerm with ta.Term, org: Origin): Result[tb.SubTerm with tb.Term -> b.Type] = x match {
        
      case x: ta.Literal[_] => super.subCoreTerm(x) map { _ -> (x match {
        case ta.UnitLit => b.types.UnitLit
        case ta.CharLit(v) => b.types.CharLit(v)
        case ta.StrLit(v) => b.types.StrLit(v)
        case ta.IntLit(v) => b.types.IntLit(v)
        case ta.RatLit(v) => b.types.RatLit(v)
      })}
        
      case ta.Id(id) => subCoreTerm(x) flatMap { sup => { // ctx =>
        //case ctx if ctx inPattern => (tb.Id(id), ctx.copy(vals = ctx.vals + (id -> newTypVar(x))))
        case ctx if ctx inPattern =>
        //if (ctx inPattern) {
          val typ = newTypVar(x)
          sup -> typ -> ctx.copy(vals = ctx.vals + (id -> typ))
        //}
        case ctx => //tb.Id(id) -> ctx.vals.get(id).getOrElse(throw CompileError("Identifier not found: "+id))
        //else {
          val typ = ctx.vals.getOrElse(id, throw CompileError("Identifier not found: "+id, Some(org)))
          sup -> typ -> ctx
        //}
      }}
        
      case ta.Atom(na,ar) => ??? //Monad.sequence(ar map snod) map { tb.Atom(na, _) }
        
      case ta.Ascribe(v, k) => for {
        v <- snod(v)
        k <- kin(k)
        _ <- (ctx: Ctx) => () -> ctx // TODO add subt cstr v.tpe <: k  
      } yield tb.Ascribe(v,k) -> k.term  
        
      case ta.Closure(pa, bo) =>
        val typ = newTypVar(ta.Id(pa))
        ctx =>
          nod(bo)(ctx) // TODO: how to add pa<:typ only for typing 'bo'?
        ???
        
    }
    
  }
  
  // FIXME rm lazy
  lazy val tconv = ???
  
  
}







// --- OLD STUFF --- when considering forking compilation

//  case class Result[+T](ctx: Ctx, inferred: Ls[T]) // FIXME Ctx should be in the list along with T!
//  implicit object Result extends Monad[Result] {
//    def lift[A](a: A) = Result(Ctx.empty, Ls(a))
//    def flatMap[A,B](ma: M[A], f: A => M[B]): M[B] = {
//      Result(ma.ctx, ma.inferred flatMap (x => f(x).inferred))
//    }
//    def map[A,B](ma: F[A], f: A => B): F[B] = Result(ma.ctx, ma.inferred map f)
//  }
//  import Result._
  
  
//  type State[+T] = Monad.State[Ctx, T]
//  val State = Monad.State[Ctx]
//  
//  type Result[+T] = Ls[T |> State]
//  implicit object Result extends Monad[Result] {
//    def lift[A](a: A) = Ls(State.lift(a))
//    def flatMap[A,B](ma: M[A], f: A => M[B]): M[B] = {
//      ma flatMap { ... } // FIXME Nope; gonna get State[Ls[T |> State]] elements, cannot flatten
//      ???
//    }
//    def map[A,B](ma: F[A], f: A => B): F[B] = ma map { (s: State[A]) => State.map(s, f) }
//  }
//  import Result._
  
  
//  /**
//    * FIXME: we would need to keep, with each ctx, a map from all traversed nodes to their inferred type
//    * 
//    */
//  type State = Ls[Ctx * b.Type]
//  val State = Monad.State[State]
//  
//  type Result[+T] = Monad.State[State, T]
//  val Result = Monad.State[State]
//  
//  import Result._
  
  
//  trait Error
//  
//  //case class Ctx()
//  case class State(valid: Ls[Ctx], invalid: Ls[Ctx * Error])
//  type Result[+T] = Monad.State[State, T]
//  val Result = Monad.State[State]
//  
//  import Result._
  







