package front2

import utils._
import common._
//import scala.text.Document

trait PrettyPrint {
//self: Terms# TermsTemplate =>
self: Terms =>
  
  case class Printer(t: TermsTemplate) {
    import t._
    import Printable._
    
    trait PrettyPrinted {
      def toDoc = ((this: Any) match { // TODO use Option()
        case x: ComStmt => print(x)
        case x: ASTStmt => print(x)
        case x: CoreStmt => print(x)
        //case x: Node => print(x)
        //case x: Printable[Any] => x.print(x)()
        case x: SelfPrintable =>
          //x.printable.print(x)()
//          val y: x.type = x
//          y.printable.print(y)()
          x.print()
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
    
    implicit val comStmtPrint: Printable[ComStmt] = Printable {
      case Let(mod, pat, bod, whe) => s"$mod let $pat = $bod" + (if (whe.isEmpty) "" else "\nwhere " + (whe mkString ";"))
    }
    implicit val astStmtPrint: Printable[ASTStmt] = Printable {
      case t.ModBlock(modifs, stmts) => ???
      case x: ComStmt => print(x)
    }
    implicit val coreStmtPrint: Printable[CoreStmt] = Printable {
      case RecBlock(stmts) => ??? //print(ModBlock(Rec::Nil, stmts))
      case x: ComStmt => print(x)
    }
    implicit val nodePrint: Printable[Node] = Printable {
      case _ => ???
    }
    
    
    
    
  }
  
}
