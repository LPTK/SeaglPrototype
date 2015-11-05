package common

import utils._

sealed trait Ident {
  override def toString = this match {
    case StableId(pat, Symbol(nam)) =>
      //"::" + (pat mkString "::") + nam
      pat mkString ("::", "::", nam)
    case LocalId(Symbol(str)) => str
    case si: SyntheticId => "$" + (si.nameHint match { // TODO use unique number ids...
      case Some(Symbol(str)) => str
      case None => "tmp"
    })
  }
}
case class StableId(path: Ls[Integer], name: Sym) extends Ident // eg: `Seagl :: Lang :: Int`
case class LocalId(name: Sym) extends Ident
class SyntheticId(val nameHint: Opt[Sym] = None) extends Ident

