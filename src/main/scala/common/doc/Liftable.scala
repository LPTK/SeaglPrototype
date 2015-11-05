package common.doc

import scala.annotation.implicitNotFound

@implicitNotFound("Cannot find document.Liftable implementation for type ${T}")
trait Liftable[-T] {
  def apply(value: T): Document
}

object Liftable {

  def apply[T](f: T => Document): Liftable[T] =
    new Liftable[T] { def apply(value: T): Document = f(value) }

}
