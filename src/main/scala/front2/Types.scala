package front2

import utils._
import common._
import Stages2._

object Types {
  
  sealed trait TypeKind
  case object StarKind extends TypeKind
  case object AtKind extends TypeKind
  case class Arrow(from: TypeKind, to: TypeKind) extends TypeKind
  
  /**
   * Abstract Arrow:
   * Some HK types, declared private, are viewd as abtract HK arrows from the outside, and kept as atomic types (not
   * reduced to their underlying definition). That is why we need to know their variance.
   * 
   */
  case class AbsArrow(from: TypeKind, to: TypeKind, vari: Variance) extends TypeKind
  
  
  sealed trait Variance
  case object Covariant extends Variance
  case object Contravariant extends Variance
  case object Invariant extends Variance
  
  
  
//  import Typed._
//  
//  /** Reduces all redexes present in a type term */
//  def normalize(tp: Type): Type = ???
//  
//  /** Inlines one inline-able external definition; returns None if nothing more to inline */
//  def step(tp: Type): ?[Type] = ???
  
  
}

