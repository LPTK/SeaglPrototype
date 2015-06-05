
package object common {
  import scala.language.implicitConversions
  
  implicit def toPrintableContext(ctx: StringContext): PrintableContext = new PrintableContext(ctx)
  
  
}
