package front2

import common.doc._
import common.doc.Document
import common.doc.Document._
import utils._

import common._
import Stages2._

object Printer {
  val SyntheticMarker = "_"
}

/**
  * Note that a better way to do this would be to have a trait where ctors for all terms would be defined and override-able;
  * (polymorphic embedding approach)
  * then we'd just override them to describe the new construction, instead of traversing the trees manually with `for`s.
  */
abstract class Printer[A <: Stage2](val _a: A) extends StageConverter[A,Printed.type](_a,Printed) {
conv =>
  import Printer._
  
  def apply(x: a.values.Node) = vconv.nod(x)(Ctx.init)._1
  //def apply(x: a.types.Node) = tconv.nod(x) // same type after erasure...
  
  case class Ctx(
    prec: Int,
    assoc: ?[Bool],
    namedIds: SyntheticId ->? Str = ->?.empty,
    nameCounts: Str ->? Int = ->?.empty
  ) {
    
    /** */
    def mkId(syd: SyntheticId): (Document, Ctx) = namedIds get syd map text map (_ -> this) getOrElse {
      val hint = syd.nameHint getOrElse 'tmp match { case Sym(s) => SyntheticMarker+s }
      val newNum = (nameCounts getOrElse (hint, 0)) + 1
      val name = hint+newNum
      text(name) -> copy(namedIds = namedIds + (syd -> name), nameCounts = nameCounts + (hint -> newNum))
    }
    
  }
  object Ctx { lazy val init = Ctx(0, None) }
  
  type Result[+T] = Monad.State[Ctx, T]
  val Result = Monad.State[Ctx]
  import Result._
  
  //def modif(prec: Int = null, assoc: ?[Bool] = null)
  def modif(prec: Int, assoc: ?[Bool] = null) =
    (s: Ctx) => s.copy(prec = prec, assoc = ?(assoc) getOrElse s.assoc) //assoc = if (assoc == null) s.assoc else assoc)
  def modif(assoc: ?[Bool]) =
    (s: Ctx) => s.copy(assoc = assoc)
  
  
  def mod(x: a.Modif): Result[b.Modif] = ???
  
  def printMod(x: a.Modif): Result[Document]
  
  def print(x: a.AnyStmt): Result[Document] = x match {
    case Left(ts) => tconv.printStmt(ts)
    case Right(vs) => vconv.printStmt(vs)
  }
  
  val tconv: PrinterTermsConverter[a.types.type,b.types.type]
  val vconv: PrinterTermsConverter[a.values.type,b.values.type]
  
  
  //abstract class AnfTermsConverter[TA <: a.TermsTemplate](val _ta: TA) extends TermsConverter[TA,Printed.values.type](_ta,Printed.values) {
  abstract class PrinterTermsConverter[+TA <: a.TermsTemplate, +TB <: Printed.DocTerms](val _ta: TA, val _tb: TB)
  extends TermsConverter[TA,TB](_ta,_tb) {
    import ta._
    
    def print(x: Kind): Result[Document]
    //def print(x: a.Modif): Result[Document]
    
    def printStmt(x: Stmt): Result[Document]
    
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
//      case Id(SyntheticId(None)) => (s: Ctx) => text("$tmp"+s.idNum) -> s.copy(idNum = s.idNum+1)
//      case Id(SyntheticId(Some(Sym(str)))) => (s: Ctx) => text("$"+str+s.idNum) -> s.copy(idNum = s.idNum+1) // TODO
      case Id(si: SyntheticId) => (s: Ctx) => s.mkId(si)
        
      case Atom(nam, args) => for { args <- Monad.sequence(args map snod) }
        yield doc"`$nam" :: args.mkDocument(" ")
      case App(fun, arg) => for(fun <- snod(fun); arg <- snod(arg))
        yield doc"($fun $arg)"
      case DepApp(fun, arg) => for(fun <- snod(fun); arg <- co.snod(arg))
        yield doc"($fun $arg)"
      case Block(stmts, ret) => for { stmts <- Monad.sequence(stmts map conv.print); ret <- nod(ret) }
        yield "{ " :: ((stmts :+ ret) mkDocument "; ") :: " }"
      case Ascribe(v, k) => for(v <- snod(v); k <- print(k))
        yield doc"$v : $k"
    }
    
    def print(x: ASTTerm): Result[Document] = x match {
      case Lambda(pat, bod) => for(pat <- nod(pat); bod <- nod(bod))
        yield doc"($pat => $bod)"
      case LambdaCompo(lams) => for(lams <- Monad.sequence(lams map print))
        yield "{" :: lams.mkDocument(" | ") :: "}"
      case OpAppL(ar, op) => for (ar <- snod(ar)) yield doc"($ar $op)"
      case OpAppR(op, ar) => for (ar <- snod(ar)) yield doc"($op $ar)"
      case OpTerm(op) => doc"($op)" |> lift
      case x: ComTerm => print(x)
      case x: Let => print(x: ASTStmt)
      case x => scalasDumb(x)
    } //match { case d: Document => d  case (d,s) => }
    
    def print(x: CoreTerm): Result[Document] = x match {
      case x: ComTerm => print(x)
    }
    
    def print(x: ComStmt): Result[Document] = x match {
      case ta.Let(mo,pa,bo,wh) => for {
        mo <- printMod(mo)
        pa <- nod(pa)
        bo <- nod(bo)
        wh <- Monad.sequence(wh map conv.process)
      } yield doc"$mo$pa = $bo" // TODO $wh
      case _ => ???
    }
    def print(x: ASTStmt): Result[Document] = x match {
      case x: ComStmt => print(x)
    }
    def print(x: CoreStmt): Result[Document] = x match {
      case x: ComStmt => print(x)
    }
    
  }
  
}

//object ASTPrinter extends StageConverter[AST.type, Printed.type](AST, Printed) {
object ASTPrinter extends Printer[AST.type](AST) {
  import Result._
  
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
  
  def printMod(x: a.Modif): Result[Document] =
    //x.toString |> text |> lift
    {(x :+ "") mkString " "} |> text |> lift
  
  
  //val tconv: TermsConverter[AST.types.type, Printed.types.type] = new PrinterTermsConverter[a.types.type, b.types.type](AST.types, Printed.types) {
  object tconv extends PrinterTermsConverter[a.types.type, b.types.type](AST.types, Printed.types) {
    import ta._
    val co: vconv.type = vconv
    
    def nod(x: Node): Result[tb.Node] = print(x.term)
    def snod(x: SubNode): Result[tb.SubNode] = print(x.term)
    def kin(x: Kind): Result[tb.Kind] = print(x)
    def stmt(x: Stmt): Result[tb.Stmt] = ???
    
    //def print(x: a.Modif): Result[Document] = x.toString |> text |> lift
    
    def print(x: Kind) = ???
    
    def printStmt(x: Stmt): Result[Document] = print(x)
    
  }
  
  //val vconv = new PrinterTermsConverter[a.values.type, b.values.type](AST.values, Printed.values) {
  object vconv extends PrinterTermsConverter[a.values.type, b.values.type](AST.values, Printed.values) {
    import ta._
    val co: tconv.type = tconv
    
    def nod(x: Node): Result[tb.Node] = print(x.term)
    def snod(x: SubNode): Result[tb.SubNode] = print(x.term)
    def kin(x: Kind): Result[tb.Kind] = print(x)
    def stmt(x: Stmt): Result[tb.Stmt] = ???
    
    def print(x: Kind) = ???
    
    def printStmt(x: Stmt): Result[Document] = print(x)
    
  }
  
  
  //val tconv = ???
  //val vconv = ???
  
  /** WHAT A MESS AND A HEADACHE. 
    * For some absolutely obscure and insane reason, Scala REFUSES to accept any of the following BUT the last one.
    * EDIT: and now it accepts them (after introducing value/typez to circumvent a compiler crash.....).....
    */
  //val tconv: TermsConverter[AST.types.type, Printed.types.type] = ???
  //val tconv: TermsConverter[a.types.type, b.types.type] = ???
  //val tconv: TermsConverter[Nothing, b.types.type] = ???
  //val tconv: TermsConverter[a.types.type, Nothing] = ???
  
  
  
}

object DesugaredPrinter extends Printer[Desugared.type](Desugared) {
  import Result._
  
  def printMod(x: a.Modif): Result[Document] = (if (x.priv) "priv" else "") |> text |> lift
  
  //val tconv: TermsConverter[AST.types.type, Printed.types.type] = new PrinterTermsConverter[a.types.type, b.types.type](AST.types, Printed.types) {
  object tconv extends PrinterTermsConverter[a.types.type, b.types.type](Desugared.types, Printed.types) {
    import ta._
    val co: vconv.type = vconv
    
    def nod(x: Node): Result[tb.Node] = print(x.term)
    def snod(x: SubNode): Result[tb.SubNode] = print(x.term)
    def kin(x: Kind): Result[tb.Kind] = print(x)
    def stmt(x: Stmt): Result[tb.Stmt] = ???
    
    //def print(x: a.Modif): Result[Document] = x.toString |> text |> lift
    
    def print(x: Kind) = ???
    
    def printStmt(x: Stmt): Result[Document] = print(x)
    
  }
  
  //val vconv = new PrinterTermsConverter[a.values.type, b.values.type](AST.values, Printed.values) {
  object vconv extends PrinterTermsConverter[a.values.type, b.values.type](Desugared.values, Printed.values) {
    import ta._
    val co: tconv.type = tconv
    
    def nod(x: Node): Result[tb.Node] = print(x.term)
    def snod(x: SubNode): Result[tb.SubNode] = print(x.term)
    def kin(x: Kind): Result[tb.Kind] = print(x)
    def stmt(x: Stmt): Result[tb.Stmt] = ???
    
    def print(x: Kind) = ???
    
    def printStmt(x: Stmt): Result[Document] = print(x)
    
  }
  
}












