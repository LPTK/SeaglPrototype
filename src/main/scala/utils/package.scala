package object utils {
  
  type Bool = Boolean
  val Bool = Boolean
  
  type Str = String
  // Note: String is not a value
  
  type Sym = Symbol
  val Sym = Symbol
  
  type Opt[+T] = Option[T]
  val Opt = Option
  
  type Eit[+A,+B] = Either[A,B]
  val Eit = Either
  
  type =>? [A,B] = PartialFunction[A,B]
  
  
  def wtf = throw new Exception("Unexpected program state reached")
  def wth(str: => Str) = throw new Exception(s"Unexpected program state reached: str")
  
  
  implicit class Andable[T](val self: T) extends AnyVal {
    
    def and(f: T => Unit) = { f(self); self }
    
    def oh_and(f: => Unit) = { f; self }
    
//    def but_before(f: => Unit) = { f; self }
    
    def in[R] (f: T => R) = f(self)
    def in[R] (f: (T,T) => R) = f(self,self) // eg:  42 in (_ -> _)
    def in[R] (f: (T,T,T) => R) = f(self,self,self)
    
  }
  
  def enclose[T](before: => Unit, after: => Unit)(f: => T) =
    {before; f} oh_and after
  
  
  
  implicit class TypeSafeEquatable[T](private val value : T) extends AnyVal {
    def ===[U >: T <: T] (that: U) = value == that
    def ==~[U <: T] (that: U) = value == that
    def ~== (that: T) = value == that
    
    def =/=[U >: T <: T] (that: U) = value != that
    def =/~[U <: T] (that: T) = value != that
    def ~/= (that: T) = value != that
    
  }
  
  
}


