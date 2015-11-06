package front2

import utils._

import common._
import Stages2._

object Typing extends StageConverter[Desugared.type, Typed.type](Desugared, Typed) {
  
  //type Ctx = Sym ->? b.Type
  case class Ctx(typs: Sym ->? Types.TypeKind, vals: Sym ->? b.Type)
  object Ctx { val empty = Ctx(->?.empty, ->?.empty) }
  
  type Result[+T] = Monad.State[Ctx, T]
  val Result = Monad.State[Ctx]
  import Result._
  
  
  
  def mod(x: a.Modif): Result[b.Modif] = ???
  
  val vconv = ???
  
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
  







