package front2

import utils._
import common._
import Stages2._

object Types {
  
  sealed trait Kind
  
  case object AnyType extends Kind
  case object Region extends Kind
  
  case class Typ(typ: Typed.Type) extends Kind
  
  case class Arrow(from: Kind, to: Kind) extends Kind
  
  /**
   * Abstract Arrow:
   * Some HK types, declared private, are viewd as abtract HK arrows from the outside, and kept as atomic types (not
   * reduced to their underlying definition). That is why we need to know their variance.
   * -- in fact, we need this for newtypes
   * 
   */
  case class AbsArrow(from: Kind, to: Kind, vari: Variance) extends Kind
  
  case class Concept(defs: Ls[Def])
  
  
  sealed trait Def
  case class TypDef() extends Def
  case class ValDef() extends Def
  
  
  
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

