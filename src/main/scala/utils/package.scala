package object utils {
  import scala.language.higherKinds
  
  type Bool = Boolean
  val Bool = Boolean
  
  type Str = String
  // Note: String is not a value
  
  type Sym = Symbol
  val Sym = Symbol
  
  type Ls[+T] = List[T]
  val Ls = List
  
  type *[+A,+B] = (A,B)
  type ->[+A,+B] = (A,B) // IntelliJ seems to be bugged by *, although it seems to be valid Scala
  
  type Opt[+T] = Option[T]
  val Opt = Option
  val So = Some
  val No = None
  type ?[+T] = Option[T]
  val ? = Option
  
  type Eit[+A,+B] = Either[A,B]
  val Eit = Either
  val Le = Left
  val Ri = Right
  type | [+A,+B] = Either[A,B]
  
  type =>? [A,B] = PartialFunction[A,B]
//  type ?=> [A,B] = PartialFunction[A,B]
  val Partial = PartialFunction
  
  //type -> [-A,+B] = A => B
  type ->? [A,+B] = Map[A,B]
  val ->? = Map
  type ->* [A,B] = collection.mutable.MultiMap[A,B]
//  val ->* = collection.mutable.MultiMap 
//  type ?-> [A,+B] = Map[A,B]
//  type ?->* [A,B] = collection.mutable.MultiMap[A,B]
  
  val Iterate = Iterator // eg, Iterate continually 42 
  
  object -> {
    def unapply[A,B](x: (A,B)) = Some(x)
  }
  
  def wtf = throw new Exception("Unexpected program state reached")
  def wtf(str: => Str) = throw new Exception(s"Unexpected program state reached: $str")
//  def wth(str: => Str) = throw new Exception(s"Unexpected program state reached: str")
  
  def that = new {
    def shit (msg: Str) = throw new Exception("Something " + msg)
  }
  
  
  // Useful free-standing functions
  
  def not(b: Bool) = !b
  
  def some[A](a: A) = Some(a)
  def none = None
  
  def left[A](a: A) = Left(a)
  def right[A](a: A) = Right(a)
  
  
  implicit class Andable[T](val __self: T) extends AnyVal {
    
    def and(f: T => Unit) = { f(__self); __self }
    
    def oh_and(f: => Unit) = { f; __self }
    
//    def but_before(f: => Unit) = { f; self }
    
    def in[R] (f: T => R) = f(__self)
    def in[R] (f: (T,T) => R) = f(__self,__self) // eg:  42 in (_ -> _)
    def in[R] (f: (T,T,T) => R) = f(__self,__self,__self)
    
  }
  
  
  type |> [A, B[_]] = B[A]
  type <| [A[_], B] = A[B]
  
  implicit class GenHelper[A](val __self: A) extends AnyVal {
    def |> [B] (rhs: A => B): B = rhs(__self)
    def into [B] (rhs: A => B): B = rhs(__self)
    /**A lesser precedence one! */ def /> [B] (rhs: A => B): B = rhs(__self)
//  }
//  implicit class RightAssocApp[A, B](self: A => B) extends AnyVal {
//  implicit class AppFun[A](val __self: A) extends AnyVal {
//    def >>: (that: A): B = self(that)
    /** 
     * A helper to write left-associative applications, mainly used to get rid of paren hell
     * Example:
     *   println(Id(Sym(f(chars))))
     *   println(Id |: Sym.apply |: f |: chars)  // `Sym` needs `.apply` because it's overloaded
     */
    def <|: [B] (lhs: A => B): B = lhs(__self)
//    def >>: [B] (lhs: {def apply(x: A): B}): B = lhs(rhs)
    
    def ==> (rhs: Bool)(implicit ev: A <:< Bool) = !__self || rhs
  }
  implicit class FunHelper[A,B](val __self: A => B) extends AnyVal {
    def <| (rhs: A): B = __self(rhs)
    def taking(rhs: A): B = __self(rhs)
    def |>: (lhs: A): B = __self(lhs)
  }
  
  def enclose[T](before: => Unit, after: => Unit)(f: => T) =
    {before; f} oh_and after
  
  
  implicit class PredNeg[T](f: T => Boolean) {
    def unary_! : T => Boolean = !f(_)
  }
  
  
  implicit class TypeSafeEquatable[T](private val value : T) extends AnyVal {
    def ===[U >: T <: T] (that: U) = value == that
    def ==~[U <: T] (that: U) = value == that
    def ~== (that: T) = value == that
    
    def =/=[U >: T <: T] (that: U) = value != that
    def =/~[U <: T] (that: T) = value != that
    def ~/= (that: T) = value != that
    
  }
  
  implicit class SymStrGet(val self: Sym) extends AnyVal {
    def getStr = self match { case Symbol(name) => name }
  }
  
}


