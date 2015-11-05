
import utils._

package object common {
  import scala.language.implicitConversions
  
  implicit def toPrintableContext(ctx: StringContext): PrintableContext = new PrintableContext(ctx)
  
  
  def warn(msg: Str) = System.err.println("Warning: "+msg)
  
}
