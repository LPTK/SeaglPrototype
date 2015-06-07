package common

import scala.util.parsing.input.Position
import utils._

trait Origin {
  
}
case class SourceCode(p: Position)
case class Synthetic(creator: Str, metaorigin: Origin)

