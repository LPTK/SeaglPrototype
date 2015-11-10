package front2

import common.Ident
import common.Stage2
import utils._

//abstract class ScopingConverter[A <: Stage2, B <: Stage2](_a: A, _b: B) extends StageConverter(_a,_b) {
trait ScopingConverter[A <: Stage2, B <: Stage2] { self: StageConverter[A,B] =>
  
  type TypeSymbol
  type ValueSymbol
  
  //case class Ctx(typs: Sym ->? TypeSymbol, vals: Sym ->? ValueSymbol, inPattern: Bool = false)
  case class Ctx(typs: Ident ->? TypeSymbol, vals: Ident ->? ValueSymbol, inPattern: Bool = false)
  object Ctx { val empty = Ctx(->?.empty, ->?.empty) }
  
  def updateCtx[A](x: Result[A])(f: Ctx => Ctx): Result[A]
  
  def regTyp(id: Ident): TypeSymbol
  def regVal(id: Ident): ValueSymbol
  
  trait ScopingTermsConverter[+TA <: a.TermsTemplate, +TB <: b.TermsTemplate] extends TermsConverter[TA,TB] { //{ self: TermsConverter[TA,TB] =>
    import Result._
    
    override def process(x: ta.ComTerm): Result[tb.ComTerm] = x match {
      //case ta.Id(id) => updateCtx(super.process(x)) { ctx => ctx.copy(typs = ctx.typs) }
      case ta.Id(id) => updateCtx(super.process(x)) { ctx => self match {
        case Left(_) => ctx.copy(typs = ctx.typs + (id -> id /> regTyp))
        case Right(_) => ctx.copy(vals = ctx.vals + (id -> id /> regVal))
      }}
      case _ => super.process(x)
    }
    
  }
  
}



