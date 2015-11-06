package front2

import common.doc.Document
import common.doc.Document._
import utils._

import common._
import Stages2._

abstract class Printer[A <: Stage2](val _a: A) extends StageConverter[A,Printed.type](_a,Printed) {
  
  //case class Ctx(curPrec: Int, curAssoc: ?[Bool], idNum: Int = 0)
  case class Ctx(prec: Int, assoc: ?[Bool], idNum: Int = 0)
  //object Ctx { val empty = Ctx() }
  
  type Result[+T] = Monad.State[Ctx, T]
  val Result = Monad.State[Ctx]
  import Result._
  
  //def modif(prec: Int = null, assoc: ?[Bool] = null)
  def modif(prec: Int, assoc: ?[Bool] = null) =
    (s: Ctx) => s.copy(prec = prec, assoc = ?(assoc) getOrElse s.assoc) //assoc = if (assoc == null) s.assoc else assoc)
  def modif(assoc: ?[Bool]) =
    (s: Ctx) => s.copy(assoc = assoc)
  
  
  def mod(x: a.Modif): Result[b.Modif] = ???
  
  //abstract class AnfTermsConverter[TA <: a.TermsTemplate](val _ta: TA) extends TermsConverter[TA,Printed.values.type](_ta,Printed.values) {
  abstract class AnfTermsConverter[TA <: a.TermsTemplate, TB <: Printed.DocTerms](val _ta: TA, val _tb: TB)
  extends TermsConverter[TA,TB](_ta,_tb) {
    
    def print(x: ta.ComTerm): Result[Document] = x match {
      case ta.Literal(v) => text(v toString) |> lift
//      case ta.Id(si: StableId) => text(si toString) |> lift    // (Sym(str))
//      case ta.Id(li: LocalId) => text(li toString) |> lift
      case ta.Id(id @ StableId(_,_) | LocalId(_)) => text(id toString) |> lift
      case ta.Id(SyntheticId(None)) => (s: Ctx) => text("$tmp"+s.idNum) -> s.copy(idNum = s.idNum+1)
      case ta.Id(SyntheticId(Some(Sym(str)))) => ??? // TODO
      case _ => empty |> lift // TODO
    }
    
    
  }
  
}

//object ASTPrinter extends StageConverter[AST.type, Printed.type](AST, Printed) {
object ASTPrinter extends Printer[AST.type](AST) {
  
  object vconv extends AnfTermsConverter[a.values.type, b.values.type](a.values, b.values) {
    
    val co: TermsConverter[ta.dualWorld.type, tb.dualWorld.type] = tconv
    //val co: TermsConverter[ta.DualWorld, tb.DualWorld]
    
    def nod(x: ta.Node): Result[tb.Node] = ???
    def snod(x: ta.SubNode): Result[tb.SubNode] = ???
    def kin(x: ta.Kind): Result[tb.Kind] = ???
    def stmt(x: ta.Stmt): Result[tb.Stmt] = ???
    
  }
  
  val tconv = ???
  
  
  
}














