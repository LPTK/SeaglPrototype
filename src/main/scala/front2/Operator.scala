package front2

import utils._
import common._

sealed trait Operator {
  def name: Str
  
  def id: Ident
  
  def duckTyped = name.last == '?'
}

case class SymbolOperator(chars: String) extends Operator {
  require(chars.length > 0)
  def name: Str = chars
  def id = LocalId(chars |> Sym.apply)
}

case class MethodOperator(name: String) extends Operator {
  require(name.length > 0)
  def chars = "." + name
  def id = LocalId(name |> Sym.apply)
}

