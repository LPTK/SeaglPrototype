package common

import scala.language.higherKinds

import utils._

trait Converter {
  
  type Result[+T]
  implicit val Result: Monad[Result]
  
}

trait Transformer extends Converter {
  
  type Result[+T] = T
  val Result = Monad.IdMonad
  
}

trait Traverser extends Converter {
  
  type Result[+T] = Unit
  val Result = Monad.UnitMonad
  
}

/**
  * May need to define a Wrapper type and make everything handle {{{Result[Wrapper[T]]}}} instead of {{{Result[T]}}}
  * 
  * But how to encode a State monad?
  *   -> the State is simply the wrapper... everything should work fine
  * 
  * 
  */
trait Aggregator extends Converter {
  
  type Inner[+T]
  implicit val Inner: Monad[Inner]
  import Inner._
  
  def get[A](in: Inner[A]): A 
  
  type Result[+T] = Ls[Inner[T]]
  //val Result = Monad.ListMonad
  object Result extends Monad[Result] {
    def apply[T](xs: Inner[T]*) = Ls(xs: _*)
    def lift[A](a: A) = Result(Inner.lift(a))
    def flatMap[A,B](ma: M[A], f: A => M[B]): M[B] = //???
    {
      ma flatMap { x => Inner.map(x, f) |> get }
    }
    def map[A,B](ma: F[A], f: A => B) : F[B] = //Result(ma map {x => x map f})
      //Result(ma map { x => Inner.map(x, f) }: _*)
      ma map { x => Inner.map(x, f) }
  }
  
}

/** Not necessarily useful as soon as Converter gets a Ctx (for flexibility) */
trait Stateful extends Converter {
  
  type State
  
//  type Result[+T] = State => (T, State)
//  object Result extends Monad[Result] {
//    def lift[A](a: A) = (s: State) => (a, s)
//    def map[A,B](ma: F[A], f: A => B) : F[B] = (s: State) => {
//      val (v, ns) = ma(s)
//      (f(v), ns)
//    }
//    def flatMap[A,B](ma: M[A], f: A => M[B]): M[B] = (s: State) => {
//      val (v, ns) = ma(s)
//      f(v)(ns)
//    }
//  }
  type Result[+T] = Monad.State[State, T]
  val Result = Monad.State[State]
  
  
}





























