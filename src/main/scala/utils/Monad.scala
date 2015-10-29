package utils

import scala.language.higherKinds

trait Monad[MP[_]] extends Functor[MP] {
top =>
  type M[T] = MP[T]
  
  def lift[A](a: A): M[A]
  
  def flatMap[A,B](ma: M[A], f: A => M[B]): M[B]
  
  // place in independent object?
  implicit class MonadOps[A](self: M[A]) {
    def flatMap[B](f: A => M[B]): M[B] = top.flatMap(self, f)
  }
  
}
object Monad {
  type Discard[+_] = Unit
  
  implicit val UnitMonad = new Monad[Discard] {
    def lift[A](a: A): Discard[A] = ()
    def flatMap[A,B](ma: Discard[A], f: A => Discard[B]): Discard[B] = ()
    def map[A,B](ma: Discard[A], f: A => B): Discard[B] = ()
  }
  
  type Simple[+T] = T
  
  implicit val Simple = new Monad[Simple] {
    def lift[A](a: A): M[A] = a
    def flatMap[A,B](ma: M[A], f: A => M[B]): M[B] = f(ma)
    
    def map[A,B](ma: F[A], f: A => B): F[B] = f(ma)
  }
  
  implicit val SeqMonad = new Monad[Seq] {
    def lift[A](a: A): M[A] = Seq(a)
    def flatMap[A,B](ma: M[A], f: A => M[B]): M[B] = ma flatMap f
    
    def map[A,B](ma: F[A], f: A => B): F[B] = ma map f
  }
  
}

trait Functor[FP[_]] {
top =>
  type F[T] = FP[T]
  
  def map[A,B](ma: F[A], f: A => B): F[B]
  
  implicit class FunctorOps[A](self: F[A]) {
    def map[B](f: A => B): F[B] = top.map(self, f)
  }
  
}


