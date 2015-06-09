package common

import utils._
import Printable._

trait Printable[T] {

  def print(x: T)(implicit po: PrintOptions): String

}
object Printable {

  def apply[T](p: T => Str) = new Printable[T] {
    def print(x: T)(implicit po: PrintOptions) = p(x)
  }

  type PrintOptions = Unit

  type Document = String

  implicit val strPrintable = new Printable[Str] {
    def print(x: Str)(implicit po: PrintOptions) = x
  }

  implicit def toDoc[T: Printable](x: T): Doc = new Doc((implicitly[Printable[T]] print x)())
}

case class Doc(print: Str) // TODO use PrintOptions instead

class PrintableContext(ctx: StringContext) {

  object p {
    def apply(ps: Doc*): Document = {
      implicit val po = ()

      ctx.s(ps map (_.print): _*)
    }
  }

}
