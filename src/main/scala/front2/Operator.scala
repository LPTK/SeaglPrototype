package front2

import utils._

sealed trait Operator {
  def name: Str
  
  def duckTyped = name.last == '?'
}

case class SymbolOperator(chars: String) extends Operator {
  require(chars.length > 0)
  def name: Str = chars
}

case class MethodOperator(name: String) extends Operator {
  require(name.length > 0)
  def chars = "." + name
}

