package common

import common.doc.{DocumentContext, Document}

import scala.language.implicitConversions

package object doc {
  import Document._

  implicit def toDocumentContext(ctx: StringContext): DocumentContext = new DocumentContext(ctx)

  implicit def toDocument[T: Liftable](x: T): Document = implicitly[Liftable[T]].apply(x)

  implicit val docLift: Liftable[Document] = Liftable[Document](identity)

  implicit val intLift: Liftable[Int] = Liftable[Int](_.toString)

  implicit val strLift: Liftable[String] = Liftable[String](text)

  // /** Lifting document lists with line breaks in between is almost never what the user wants, and leads to errors */
  //  implicit def seqLift[T: Liftable]: Liftable[Seq[T]] = Liftable[Seq[T]] { ls =>
  //    val lift = implicitly[Liftable[T]].apply _
  //    (ls map lift).mkDocument()
  //  }

  implicit class ListDocumentOps(docs: Seq[Document]) {
    def mkDocument(pre: Document, sep: Document, post: Document): Document = docs.foldLeft[Document](empty) {
      (accl, currl) => accl :: (if (accl == empty) pre else sep) :: currl
    } :: post
    def mkDocument(sep: Document = empty): Document = mkDocument(empty, sep, empty)
  }

}

