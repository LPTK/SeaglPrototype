package common

import utils._

case class CyclicDependency() extends Exception("Illegal cyclic access")

//class Cyclic[+T](expr: Cyclic[T] => T, toStr: T => String = ((t:T) => t.toString)) { //extends Unique {
class Cyclic[+T](expr: Cyclic[T] => T, eqtest: Opt[(T,Any) => Bool] = None, toStr: T => String = ((t:T) => t.toString)) { //extends Unique {
  def this(x: T) = this(_ => x)
  
  private val _value = expr(this)
  def value =
    if (!wasComputerYet) throw CyclicDependency()
    else _value
    
  def wasComputerYet = _value != null
  
  override def toString =
    if (_value == null) "[Cyclic in resolution]"
    else toStr(value)
  
//  /** should not be default behavior; will diverge for classes with strcl eq */
//  override def equals(x: Any) = x match {
//      case Cyclic(x) => this == x
//      case _ => this == x
//  }
  override def equals(x: Any) = (eqtest,x) match {
      case (Some(eqt), Cyclic(x)) => eqt(value, x)
      case (Some(eqt), _) => eqt(value, x)
      case _ => super.equals(x) // referential equality
  }
  
}

object Cyclic {
  import scala.language.implicitConversions
  
//  def apply[T](x: T) = new Cyclic[T](_ => x)
  def unapply[T](x: Cyclic[T]) = Some(x.value)
  
//  implicit def plain2cyclic[T](t: T) = new Cyclic[T](_ => t)
  implicit def cyclic2plain[T](c: Cyclic[T]) = c.value
  
}






