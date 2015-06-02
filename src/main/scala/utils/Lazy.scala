package utils

class Lazy[T](e: => T) {
  protected var value: T = _
  def isAvailable = value != null
  
  def get =
    if (isAvailable) value
    else { value = e; e }
  def unary_! = get
  
  def toOpt = if (isAvailable) Some(value) else None
  
  def map[U](f: T=>U) = Lazy(f(get))
  
  def flatMap[U](f: T=>Lazy[U]) = Lazy(f(get).get)
  
  override def toString =
    if (isAvailable) value.toString
    else "..?"
}
object Lazy {
  def apply[T](e: => T) = new Lazy(e)
}

/////** The only difference with Lazy is the name (stating the intent) and the implicit conversion */
///** The difference with Lazy is that cyclic evaluates the expression just after it has been constructed (along with the other objects in the cycle) */
//class Cyclic[T](e: => T) extends Lazy(e) {
//  value = e
//}
//
//object Cyclic {
//  def apply[T](e: => T) = new Cyclic(e)
//  implicit def cyclic2T[T](c: Cyclic[T]) = c.get
//}




