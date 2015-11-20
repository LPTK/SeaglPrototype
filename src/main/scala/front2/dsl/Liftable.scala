package front2
package dsl

import common._  

import scala.annotation.implicitNotFound

@implicitNotFound("Cannot find document.Liftable implementation for type ${T}")
trait Liftable[-T] {
  def apply(value: T): Stages2.Desugared.values.Node      
}

object Liftable {

  def apply[T](f: T => Node): Liftable[T] =
    new Liftable[T] { def apply(value: T): Node = f(value) }

}
