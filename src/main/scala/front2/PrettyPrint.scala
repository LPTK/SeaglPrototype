package front2

import utils._
import common._
//import scala.text.Document

/**
  * TODO:
  *   use Document
  *   auto-parenthesize (use precedence values)
  *   use context to generate fresh ids
  * 
  * TODO: make it a StageConverter instead!!
  *   state monad to pass id generation number
  * 
  * 
  */
trait PrettyPrint {
//self: Terms# TermsTemplate =>
self: Terms =>
  
  case class Printer(t: TermsTemplate) {
    import t._
    import Printable._
    
    trait PrettyPrinted {
      def toDoc = ((this: Any) match { // TODO use Option()
        //case x: Printable[Any] => x.print(x)()
          //x.printable.print(x)()
//          val y: x.type = x
//          y.printable.print(y)()
        case x: SelfPrintable =>
          x.print()
        case x: ComStmt => print(x)
        case x: ASTStmt => print(x)
        case x: CoreStmt => print(x)
        case x: ComTerm => print(x)
        case x: ASTTerm => print(x)
        case x: CoreTerm => print(x)
        //case x: Node => print(x)
        case _ => null //warn("Undefined printing for "+this); super.toString
      }) |> Option.apply
      override def toString = toDoc match {
        case Some(d) => d.toString
        case None => warn(s"Undefined printing for ${super.toString} of class $getClass");
          this match {
            case self: Product => Printable.printProduct(self) //s"${getClass.getSimpleName}(${self.productIterator mkString ", "})"
            case _ => super.toString
          }
      }
    }
    
    
//    def print(x: Modif) = x match {
//      case mods: Ls[_ /*Modifier*/] => mods mkString " "
//      case Modification(priv) => if (priv) "priv" else ""
//    }
//    
//    def print(x: ComStmt) = x match {
//      case Let(mod, pat, bod, whe) => s"$mod let $pat = $bod" + (if (whe.isEmpty) "" else "\nwhere " + (whe mkString ";"))
//    }
//    def print(x: ASTStmt) = x match {
//      case t.ModBlock(modifs, stmts) => ??? 
//    }
//    def print(x: CoreStmt) = x match {
//      case RecBlock(stmts) => ??? //print(ModBlock(Rec::Nil, stmts))
//    }
//    def print(x: ComTerm) = x match {
//      case _ => ???
//    }
    
    
//    implicit val mdPrint: Printable[Metadata] = Printable {
//      case org: Origin => org.toString
//    }
    
    implicit val modifPrint: Printable[Modif] = Printable {
      case mods: Ls[_ /*Modifier*/] => (mods :+ "") mkString " "
      case Modification(priv) => if (priv) "priv" else ""
    }
    
    implicit val comStmtPrint: Printable[ComStmt] = Printable { // TODO use doc"" interpolation
      case Let(mod, pat, bod, whe) => s"${print(mod)}$pat = $bod" + (if (whe.isEmpty) "" else "\nwhere " + (whe mkString ";"))
      case Impure(n) => ??? //print(n)
    }
    implicit val astStmtPrint: Printable[ASTStmt] = Printable {
      case t.ModBlock(modifs, stmts) => ???
      case x: ComStmt => print(x)
    }
    implicit val coreStmtPrint: Printable[CoreStmt] = Printable {
      case RecBlock(stmts) => ??? //print(ModBlock(Rec::Nil, stmts))
      case x: ComStmt => print(x)
    }
//    implicit val nodePrint: Printable[Node] = Printable {
//      case _ => ???
//    }
    
    implicit val comTermPrint: Printable[ComTerm] = Printable {
      case Literal(v) => v.toString
      case Id(id) => id.toString
      case Atom(nam, args) => s"`$nam" + (args mkString " ")
      case App(fun, arg) => s"($fun $arg)"
      case DepApp(fun, arg) => s"($fun $arg)"
      case Block(stmts, ret) =>
        //s"{${stmts map (_.fold(identity,identity)) mkString "; "}$ret}"
        "{ " + ({stmts map (_.fold(identity,identity))} :+ ret mkString "; ") + " }"
      case Ascribe(v, k) => s"$v : $k"
    }
    implicit val astTermPrint: Printable[ASTTerm] = Printable {
      case t.Lambda(pat, bod) => s"($pat => $bod)"
      case t.LambdaCompo(lams) => "{"+(lams mkString " | ")+"}" //(lams map {x => print(x)()(astTermPrint)})
      case t.OpAppL(ar, op) => s"($ar $op)"
      case t.OpAppR(op, ar) => s"($op $ar)"
      case t.OpTerm(op) => s"($op)"
      case x: ComTerm => print(x)
      case x => scalasDumb(x)
    }
    implicit val coreTermPrint: Printable[CoreTerm] = Printable {
      case Closure(par, bod) => s"$par => $bod"
      case x: ComTerm => print(x)
    }
    
    
    
    
  }
  
}
