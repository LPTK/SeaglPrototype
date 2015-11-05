package common

import utils._
import Printable._

trait Printable[T] {

  def print(x: T)(implicit po: PrintOptions): Doc //String

}
//trait SelfPrintable {
//  val printable: Printable[this.type]
//  def print = printable.print(this: this.type)()
//}
trait SelfPrintable {
  def print(implicit po: PrintOptions): Doc //Str
}
trait SelfProductPrintable extends SelfPrintable { self: Product =>
  def print(implicit po: PrintOptions): Doc = printProduct(self)
}
object Printable {

  def apply[T](p: T => Doc) = new Printable[T] {
    def print(x: T)(implicit po: PrintOptions) = p(x)
  }

  type PrintOptions = Unit

  type Document = String

  implicit val strPrintable: Printable[Str] = new Printable[Str] {
    def print(x: Str)(implicit po: PrintOptions) = Doc(x)
  }
  implicit def selfPrintable[T <: SelfPrintable]: Printable[T] = Printable{ _.print() }
  
  import scala.language.implicitConversions
  implicit def toDoc[T: Printable](x: T): Doc = (implicitly[Printable[T]] print x)()//new Doc((implicitly[Printable[T]] print x)())
  def print[T: Printable](x: T) = toDoc(x)
  
  def printProduct(self: Product) = s"${self.getClass.getSimpleName}(${self.productIterator mkString ", "})"
}

case class Doc(print: Str, md: Metadata = Metadata.empty) { // TODO use PrintOptions instead
  override def toString = print
}

class PrintableContext(ctx: StringContext) {

  object p {
    def apply(ps: Doc*): Doc = {
      implicit val po = ()

      ctx.s(ps map (_.print): _*)
    }
  }

}

class Metadata(elts: Str =>? Metadata.Entry)
object Metadata {
  import scala.language.existentials
  
  val empty = new Metadata(Map.empty)
  def apply(elts: Entry*) = new Metadata(elts map (e => (e.name, e)) toMap)
  
  trait Entry { def name: Str }
  //case class Type(typ: front2.Terms# TermsTemplate# Type)
  case class Type(typ: Stage2# Type) extends Entry { def name = "Type" }
  //case class Type[S <: Stage2](typ: S# Type)
  case class Kind(typ: front2.Types.TypeKind) extends Entry { def name = "Kind" }
}










