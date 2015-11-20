package common

import utils._

class CompileError(val msg: Str, val org: ?[Origin]) extends Exception(msg)
object CompileError {
  def apply(msg: Str, pos: ?[Origin] = None) = new CompileError(msg, pos)
  def unapply(ce: CompileError) = Some(ce.msg, ce.org)
}
