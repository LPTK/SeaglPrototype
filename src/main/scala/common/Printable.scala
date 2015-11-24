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
  
  val showMetadata = false
  //val showMetadata = true
  
  def apply[T](p: T => Doc) = new Printable[T] {
    def print(x: T)(implicit po: PrintOptions) = p(x)
  }
  
  type PrintOptions = Unit
  case class Ctx(curPrec: Int, genId: Int = 0) {
    def nextId = {
      // TODO
      Ctx(curPrec, genId + 1)
    }
  }
  
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

case class Doc(text: Str, md: Metadata = Metadata.empty) { // TODO use PrintOptions instead
  //override def toString = text + (if (Printable.showMetadata) " "+md else "")
  override def toString = if (Printable.showMetadata && !md.isEmpty) s"{$text$md}" else text
}

class PrintableContext(ctx: StringContext) {

  object p {
    def apply(ps: Doc*): Doc = {
      implicit val po = ()

      ctx.s(ps map (_.text): _*)
    }
  }

}

class Metadata(elts: Str ->? Metadata.Entry) {
  override def toString = if (isEmpty) "" else elts.iterator map {
    case (k, v) => s"$k=$v"
  //} mkString ("{", ", ", "}")
  } mkString (" | ", ", ", "")
  def isEmpty = elts.isEmpty
}
object Metadata {
  import scala.language.existentials
  
  val empty = new Metadata(Map.empty)
  def apply(elts: Entry*) = new Metadata(elts map (e => (e.name, e)) toMap)
  
  trait Entry { def name: Str } //; def isEmpty: Bool}
  //case class Type(typ: front2.Terms# TermsTemplate# Type)
  case class Type(typ: Stage2# Type) extends Entry { def name = "Type" } //; def isEmpty = false }
  //case class Type[S <: Stage2](typ: S# Type)
  case class Kind(typ: front2.Types.Kind) extends Entry { def name = "Kind" } //; def isEmpty = false }
}










