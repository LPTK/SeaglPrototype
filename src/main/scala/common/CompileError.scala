package common

import utils._

class CompileError(val msg: Str) extends Exception(msg)
object CompileError {
  def apply(msg: Str) = new CompileError(msg)
  def unapply(ce: CompileError) = Some(ce.msg)
}
