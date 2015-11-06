package front2

import common.doc.Document._
import common.doc.Liftable
import utils._
import common._

sealed trait Operator {
  def name: Str
  
  def id: Ident
  
  def duckTyped = name.last == '?'
}
object Operator {
  
  implicit val opLift: Liftable[Operator] = Liftable { _.id.toString }
//    case SymbolOperator(str) => text(str)
//  }
  
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

