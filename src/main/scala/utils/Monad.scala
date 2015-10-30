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
  
//  //def sequence[M[_]: Monad, A](ls: Seq[M[A]]): M[Seq[A]] = {
//  def sequence[M[_], A](ls: Seq[M[A]])(implicit m: Monad[M]): M[Seq[A]] = {
//    //val m = implicitly[M[_]]
//    import m._
//    ls match {
//      //case h +: t => m.flatMap(h, h => m.flatMap(sequence(t), s => h +: s))
//      case h +: t => for (h <- h; t <- sequence(t)) yield h +: t
//      //case h :: t => m.flatMap(sequence(t), s => h :: s)
//      case Seq.empty => lift(Seq.empty)
//    }
//  }
  def sequence[M[_], A](ls: Ls[M[A]])(implicit m: Monad[M]): M[Ls[A]] = {
    import m._
    ls match {
      case h :: t => for (h <- h; t <- sequence(t)) yield h :: t
      case Nil => lift(Nil)
    }
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


