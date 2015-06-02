package common

import utils._

object Unique {
  private var nb = 0
  private def nextId = { nb+=1; nb-1 }
}
trait Unique {
  private val _id = Unique.nextId;
  def id = _id
  final override def equals(that: Any) = that match {
    case u: Unique => u.id === id
    case _ => false
  }
}



