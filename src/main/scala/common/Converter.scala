package common

import scala.language.higherKinds

import utils.Monad

trait Converter {
  
  type Result[+T]
  implicit val Result: Monad[Result]
  
}

trait Transformer extends Converter {
  
  type Result[+T] = T
  val Result = Monad.TrivialMonad
  
}

trait Traverser extends Converter {
  
  type Result[+T] = Unit
  val Result = Monad.UnitMonad
  
}




