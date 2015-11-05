
import utils._

package object common {
  import scala.language.implicitConversions
  
  implicit def toPrintableContext(ctx: StringContext): PrintableContext = new PrintableContext(ctx)
  
  
  def warn(msg: Str) = System.err.println("Warning: "+msg)
  
  /** Often, Scala produces false positives when checking for exhaustiveness; I use this when I believe it is the case */
  def scalasDumb = wtf("Unchecked pattern matching failed")
  
}
