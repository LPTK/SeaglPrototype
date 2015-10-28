package front2

import utils._
import common._
import Stages2._

object Types {
  
  sealed trait TypeKind
  case object StarKind extends TypeKind
  case object AtKind extends TypeKind
  case class Arrow(from: TypeKind, to: TypeKind) extends TypeKind
  
  
  import Typed._
  
  /** Reduces all redexes present in a type term */
  def normalize(tp: Type): Type = ???
  
  /** Inlines one inline-able external definition; returns None if nothing more to inline */
  def step(tp: Type): ?[Type] = ???
  
}

