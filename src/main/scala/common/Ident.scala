package common

import utils._

sealed trait Ident {
  override def toString = this match {
    case StableId(path, Symbol(nam)) =>
      //"::" + (path mkString "::") + nam
      path map (_ getStr) mkString ("", "::", "::"+nam)
    case LocalId(Symbol(str)) => str
//    case si: SyntheticId => "SynId[" + (si.nameHint match { // TODO use unique number ids...
//      case Some(Symbol(str)) => str
//      case None => "?"
//    }) + "]"
    case si: SyntheticId => super.toString //s"SynId[$hashCode]" 
  }
} 
case class StableId(path: Ls[Sym], name: Sym) extends Ident // eg: `Seagl :: Lang :: Int`
case class LocalId(name: Sym) extends Ident
class SyntheticId(val nameHint: Opt[Sym] = None) extends Ident
object SyntheticId {
  def unapply(sy: SyntheticId) = Some(sy.nameHint)
}
