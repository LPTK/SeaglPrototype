//package front
//
//import common.Stages.PretypedStage
//import utils._
//import common._
//
//import collection.mutable
//
//trait Scopes {
//stage: Stage with PretypedStage =>
//  
//  trait Scope {
//  self =>
//    import stage.{values => v, types => t}
//    
//    def term: stage.GeneralTerm
//    
//    private[Scopes] var parent: Option[Scope] = None
//    
//    val defs = term match {
//      case _: ScopingNode =>
//        val (vdefs, tdefs) = (mutable.Map[VId, stage.ValueNode](), mutable.Map[TId, stage.TypeNode]())
//        object Scoper extends StageTraverser[stage.type](stage) {
//          type Ctx = Unit
//          
//          override def vnods(x: a.ValueNode)(implicit c: Ctx) {
//            x.parent = Some(self)
//            if (x.transparent) processVal(x.term)
//          }
//          override def tnods(x: a.TypeNode)(implicit c: Ctx) {
//            x.parent = Some(self)
//            if (x.transparent) processTyp(x.term)
//          }
//          
//          override def processValSym(x: a.ValueSymbol)(implicit c: Ctx) = { x.scope(self); super.processValSym(x) }
//          override def processTypSym(x: a.TypeSymbol)(implicit c: Ctx) = { x.scope(self); super.processTypSym(x) }
//        }
//        Scoper.process(term)()
//        Some(vdefs.toMap, tdefs.toMap)
//      case _ => None
//    }
//    val transparent: Bool = !defs.isDefined
//    
//    def get(name: VId): ValueNode = (defs flatMap {case(v,t) => v get name}, parent) match {
//      case (Some(v), _) => v
//      case (None, Some(p)) => p get name
//      case (None, None) => throw CompileError("Value identifier not found: "+name) // TODO better error
//    }
//    def get(name: TId): TypeNode = (defs flatMap {case(v,t) => t get name}, parent) match {
//      case (Some(v), _) => v
//      case (None, Some(p)) => p get name
//      case (None, None) => throw CompileError("Type identifier not found: "+name) // TODO better error
//    }
//    
//  }
//  
//  
//}
//
