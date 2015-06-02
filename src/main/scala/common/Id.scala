package common

import utils._

abstract class Id(name: Str) {
  def sym: Symbol
  def fullStr = s"$name "+toString
  override def toString = sym match { case Symbol(str) => str }
}
object Id {
  implicit def toStr(id: Id) = id.toString
}

case class TId(sym: Sym) extends Id("Type")
object TId { def apply(str: Str) = new TId(Sym(str)) }

case class VId(sym: Sym) extends Id("Value")
object VId { def apply(str: Str) = new VId(Sym(str)) }

trait Uid extends Unique {
  override def toString = s"[$id]"
}
class TUid extends Uid
class VUid extends Uid

