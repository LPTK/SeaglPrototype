package front2

import scala.language.implicitConversions

import common._  

package object dsl {
  
  type Node = Stages2.Desugared.values.Node
  
  implicit def toANFContext(ctx: StringContext): ANFContext = new ANFContext(ctx)

  implicit def toANF[T: Liftable](x: T): Node = implicitly[Liftable[T]].apply(x)

  implicit val docLift: Liftable[Node] = Liftable[Node](identity)

  // TODO:
  //implicit val intLift: Liftable[Int] = Liftable[Int](_.toString)
  //
  //implicit val strLift: Liftable[String] = Liftable[String](text)
  //
  //implicit val symLift: Liftable[Symbol] = Liftable[Symbol]{ case Symbol(str) => text(str) }
  
}

