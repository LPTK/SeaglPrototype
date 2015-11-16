package front2

import utils.Monad.State
import utils._

import common._
import Stages2._

object Typing extends StageConverter[Desugared.type, Typed.type](Desugared, Typed) {
  
  //type Ctx = Sym ->? b.Type
  case class Ctx(typs: Ident ->? Types.TypeKind, vals: Ident ->? b.Type, inPattern: Bool = false)
  object Ctx { val empty = Ctx(->?.empty, ->?.empty) }
  
  type Result[+T] = Monad.State[Ctx, T]
  val Result = Monad.State[Ctx]
  import Result._
  
  
  def mod(x: a.Modif): Result[b.Modif] = ???
  
  
  def newTypVar(nameHint: Desugared.TermsTemplate# CoreTerm): b.Type = {
    ???
  }
  //def md(org: Origin, typ: b.Type): b.values.Metadata = ???
  
  //val vconv = ???
  object vconv extends TermsConverterClass[a.values.type, b.values.type](Desugared.values, Typed.values) with ValueConverter {
  
    def nod(x: vconv.ta.Node): State[Ctx, vconv.tb.Node] = //process(x.term) map {tb.Node(_, typeinfer(x.term))}
      for {
        typ <- typeinfer(x.term)
        t <- process(x.term)
      } yield tb.Node(t, (x.md, typ))
    
    def snod(x: vconv.ta.SubNode): State[Ctx, vconv.tb.SubNode] =
      for {
        typ <- typeinfer(x.term)
        t <- subCoreTerm(x.term) //: ta.SubTerm)
      } yield tb.SubNode(t, (x.md, typ))
  
    def kin(x: Desugared.types.Node): State[Ctx, Typed.types.Node] = ???
    
    //def mod(x: ta.Modif): Result[tb.Modif]
    def stmt(x: ta.Stmt): State[Ctx, tb.Stmt] = ???
    
    //
    
    //override def process(x: ta.ComTerm) = x match {
    def typeinfer(x: ta.Term): Result[b.Type] = x match {
      case ta.Id(id) => {
        //case ctx if ctx inPattern => (tb.Id(id), ctx.copy(vals = ctx.vals + (id -> newTypVar(x))))
        case ctx if ctx inPattern =>
          val typ = newTypVar(x)
          typ -> ctx.copy(vals = ctx.vals + (id -> typ))
        case ctx => //tb.Id(id) -> ctx.vals.get(id).getOrElse(throw CompileError("Identifier not found: "+id))
          val typ = ctx.vals.getOrElse(id, throw CompileError("Identifier not found: "+id))
          typ -> ctx
      }
      case ta.App(a, b) =>
        // infer new typle class cstr for every typvar?
        
        ???
      case _ => ??? // TODO //super.process(x)
    }
    
  }
  
  val tconv = ???
  
  
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
  







