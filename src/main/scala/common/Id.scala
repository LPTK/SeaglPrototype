package common

import scala.language.implicitConversions

import utils._

abstract class Id(kind: Str) {
  def sym: Symbol
  def fullStr = s"$kind "+toString
  override def toString = sym match { case Symbol(str) => str }
}
object Id {
  implicit def toStr(id: Id): Str = id.toString
}

case class TId(sym: Sym) extends Id("Type")
object TId { def apply(str: Str) = new TId(Sym(str)) }

case class VId(sym: Sym) extends Id("Value")
object VId { def apply(str: Str) = new VId(Sym(str)) }

abstract class Named(kind: Str) { def name: Str; override def toString() = s"$kind($name)" }
class TSym(val name: Str) extends Named("Typ") with Unique
class VSym(val name: Str) extends Named("Val") with Unique
