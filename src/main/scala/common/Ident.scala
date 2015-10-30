package common

import utils._

sealed trait Ident
case class StableId(path: Ls[Integer], name: Sym) extends Ident // eg: `Seagl :: Lang :: Int`
case class LocalId(name: Sym) extends Ident
class SyntheticId(val nameHint: Opt[Sym] = None) extends Ident

