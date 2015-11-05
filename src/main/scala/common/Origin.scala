package common

import scala.util.parsing.input.Position
import utils._

trait Origin extends Metadata.Entry {
  def name = "Origin"
  
  def + (that: Origin) = MixedOrg(this, that)
  
  override def toString = this match {
    case SourceCode(pos) => pos.toString()//"[]"
    case MixedOrg(orgs @ _*) => orgs mkString ("[",";","]")
    case Synthetic(cre, met) => met.fold("[$cre]")(o => s"[$cre ($o)]")
  }
}
case class MixedOrg(orgs: Origin*) extends Origin
case class SourceCode(pos: Position) extends Origin
case class Synthetic(creator: Str, metaorigin: ?[Origin]) extends Origin

