package common

import scala.util.parsing.input.Position
import utils._

trait Origin extends Metadata.Entry {
  def name = "Origin"
  
  def + (that: Origin) = MixedOrg(this, that)
  
}
case class MixedOrg(orgs: Origin*) extends Origin
case class SourceCode(pos: Position) extends Origin
case class Synthetic(creator: Str, metaorigin: ?[Origin]) extends Origin

