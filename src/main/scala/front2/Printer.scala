package front2

import common.doc._
import common.doc.Document
import common.doc.Document._
import utils._

import common._
import Stages2._

abstract class Printer[A <: Stage2](val _a: A) extends StageConverter[A,Printed.type](_a,Printed) {
conv =>
  
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
  abstract class PrinterTermsConverter[+TA <: a.TermsTemplate, +TB <: Printed.DocTerms](val _ta: TA, val _tb: TB)
  extends TermsConverter[TA,TB](_ta,_tb) {
    import ta._
    
    /*
    def print(x: ComTerm): Result[Document] = (s: Ctx) => x match {
      case Literal(v) => text(v toString) -> s //|> lift
      case Id(id @ (StableId(_,_) | LocalId(_))) => text(id toString) -> s //|> lift
      case Id(SyntheticId(None)) => /*(s: Ctx) =>*/ text("$tmp"+s.idNum) -> s.copy(idNum = s.idNum+1)
      case Id(SyntheticId(Some(Sym(str)))) => /*(s: Ctx) =>*/ text("$"+str+s.idNum) -> s.copy(idNum = s.idNum+1) // TODO
        
      case Atom(nam, args) =>
        val f = Monad.sequence(args map snod)
        val args2 -> s2 = f(s)
        (doc"`$nam" :: args2.mkDocument(" ")) -> s2
//        for {
//          args <- f(s)
//        } yield (doc"`$nam" :: args.mkDocument(" "))
        
      case App(fun, arg) => text(s"($fun $arg)") -> s
      case DepApp(fun, arg) => text(s"($fun $arg)") -> s
      case Block(stmts, ret) =>
        //s"{${stmts map (_.fold(identity,identity)) mkString "; "}$ret}"
        text("{ " + ({stmts map (_.fold(identity,identity))} :+ ret mkString "; ") + " }") -> s
      case Ascribe(v, k) => text(s"$v : $k") -> s
      //case _ => empty -> s //|> lift // TODO
    }
    */
    def print(x: ComTerm): Result[Document] = x match {
      case Literal(v) => v.toString |> text |> lift
      case Id(id @ (StableId(_,_) | LocalId(_))) => id.toString |> text |> lift
      case Id(SyntheticId(None)) => (s: Ctx) => text("$tmp"+s.idNum) -> s.copy(idNum = s.idNum+1)
      case Id(SyntheticId(Some(Sym(str)))) => (s: Ctx) => text("$"+str+s.idNum) -> s.copy(idNum = s.idNum+1) // TODO
        
      case Atom(nam, args) => for { args <- Monad.sequence(args map snod) } yield doc"`$nam" :: args.mkDocument(" ")
      case App(fun, arg) => for(fun <- snod(fun); arg <- snod(arg)) yield doc"($fun $arg)"
      case DepApp(fun, arg) => for(fun <- snod(fun); arg <- co.snod(arg)) yield doc"($fun $arg)"
      case Block(stmts, ret) => for { stmts <- Monad.sequence(stmts map conv.process) } yield doc""
    }
    
    def print(x: ASTTerm): Result[Document] = (s: Ctx) => x match {
      case Lambda(pat, bod) => s"($pat => $bod)" /> text -> s
      case LambdaCompo(lams) => ("{"+(lams mkString " | ")+"}" |> text) -> s
      case OpAppL(ar, op) => s"($ar $op)" /> text -> s
      case OpAppR(op, ar) => s"($op $ar)" /> text -> s
      case OpTerm(op) => s"($op)" /> text -> s
      case x: ComTerm => print(x)(s)
      case x => scalasDumb(x)
    } //match { case d: Document => d  case (d,s) => }
  }
  
}

//object ASTPrinter extends StageConverter[AST.type, Printed.type](AST, Printed) {
object ASTPrinter extends Printer[AST.type](AST) {
  
  /*
  //object vconv extends PrinterTermsConverter[a.values.type, b.values.type](a.values, b.values) {
  val vconv: TermsConverter[a.values.type, b.values.type] = ???
    
//    //val co: TermsConverter[ta.dualWorld.type, tb.dualWorld.type] = tconv
//    //val co: TermsConverter[ta.DualWorld, tb.DualWorld]
//    val co = ???
//    
//    def nod(x: ta.Node): Result[tb.Node] = ???
//    def snod(x: ta.SubNode): Result[tb.SubNode] = ???
//    def kin(x: ta.Kind): Result[tb.Kind] = ???
//    def stmt(x: ta.Stmt): Result[tb.Stmt] = ???
//    
//  }
  
  //val tconv: PrinterTermsConverter[a.types.type, b.types.type] = ???
  //val tconv: TermsConverter[a.types.type, b.types.type] = ???
  val tconv: TermsConverter[AST.types.type, Printed.types.type] = ???
  */
  
  
  val tconv: TermsConverter[AST.types.type, Nothing] = new PrinterTermsConverter[a.types.type, b.types.type](AST.types, Printed.types) {
    val co: vconv.type = vconv
    
    def nod(x: ta.Node): Result[tb.Node] = ???
    def snod(x: ta.SubNode): Result[tb.SubNode] = ???
    def kin(x: ta.Kind): Result[tb.Kind] = ???
    def stmt(x: ta.Stmt): Result[tb.Stmt] = ???
    
  }.asInstanceOf[TermsConverter[AST.types.type, Nothing]]
  
  val vconv = new PrinterTermsConverter[a.values.type, b.values.type](AST.values, Printed.values) {
    val co: tconv.type = tconv
    
    def nod(x: ta.Node): Result[tb.Node] = print(x.term)
    def snod(x: ta.SubNode): Result[tb.SubNode] = ???
    def kin(x: ta.Kind): Result[tb.Kind] = ???
    def stmt(x: ta.Stmt): Result[tb.Stmt] = ???
    
  }.asInstanceOf[TermsConverter[AST.values.type, Nothing]]
  
  
  //val tconv = ???
  //val vconv = ???
  
  /** WHAT A MESS AND A HEADACHE. 
    * For some absolutely obscure and insane reason, Scala REFUSES to accept any of the following BUT the last one.
    */
  //val tconv: TermsConverter[AST.types.type, Printed.types.type] = ???
  //val tconv: TermsConverter[a.types.type, b.types.type] = ???
  //val tconv: TermsConverter[Nothing, b.types.type] = ???
  //val tconv: TermsConverter[a.types.type, Nothing] = ???
  
  
  
}














