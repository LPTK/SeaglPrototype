package front

import common.Stages._
import common._
import utils._


object Presolve extends StageConverter(Ast, Resolving) {
  import collection.mutable._
  import b._
  import b.values._
  
  case class IdentifierNotFound(id: Id) extends CompileError(s"identifier not found: ${id.fullStr}")
  
  /** Will be useful with real sub-scopes where forward refs are allowed */
//  case class Ctx (typTable: Map[TId, TSym], valTable: Map[VId, VSym], parent: Option[Ctx])
//  case class Ctx (parent: Option[Ctx], typTable: Map[TId, TSym] = Map(), valTable: Map[VId, VSym] = Map())
  class Ctx (parent: Option[Ctx], typTable: Map[TId, TSym] = Map(), valTable: Map[VId, VSym] = Map())
  {
    def this(parent: Ctx) = this(Some(parent))
    
    def apply(x: TId): TSym = (typTable get x, parent) match {
      case (Some(v), _) => v
      case (_, Some(p)) => p(x)
      case _ => throw IdentifierNotFound(x)
    }
    def update(x: TId, y: TSym) = typTable(x) = y
    
    def apply(x: VId): VSym = (valTable get x, parent) match {
      case (Some(v), _) => v
      case (_, Some(p)) => p(x)
      case _ => throw IdentifierNotFound(x)
    }
    def update(x: VId, y: VSym) = valTable(x) = y
    
  }
  
  def typs(x: a.TypSym)(implicit c: Ctx) = { Lazy(c(x)) } // ult(ctx(x))
  def vals(x: a.ValSym)(implicit c: Ctx) = { Lazy(c(x)) }
  
  def vnods(x: a.ValueNode)(implicit c: Ctx) = processVal(x)
  def tnods(x: a.TypeNode)(implicit c: Ctx) = processTyp(x)

  
//  def tspec(x: a.TypeSpec) = x map apply
//  def tparam(x: a.TypeParam) = AbsTyp(new TUid, x, Seq(), Seq(), false, true) and (ctx(x) = _)
  
  /** TODO: correctly nest passed contexts */
  override def processVal(x: a.Value)(implicit c: Ctx): Value = x match {
//    case av.Let(s, _, _) => c(s) = new VSym(s.toString); super.processVal(x)
    case av.Let(s, v, b) => c(s) = new VSym(s.toString); Let(vals(s), vnods(v)(new Ctx(c)), vnods(b))
    case av.Lambda(av.Extract(a), b) => Lambda(Extract(vnods(a)), vnods(b)) // TODO: should introduce new bindings into context for b!!
    case av.Ref(s) => Ref(vals(s))
    case _ => super.processVal(x)
  }
  override def processTyp(x: a.Type)(implicit c: Ctx): Type = x match {
    case at.Let(s, v, b) => c(s) = new TSym(s.toString); t.Let(typs(s), tnods(v)(new Ctx(c)), tnods(b))
      // TODO Lambda
    case at.Ref(s) => t.Ref(typs(s))
    case _ => super.processTyp(x)
  }
  
}

object Resolve extends StageConverter(Resolving, Resolved) {
  
  def typs(x: a.TypSym)(implicit c: Ctx) = x.get
  def vals(x: a.ValSym)(implicit c: Ctx) = x.get
  
  def vnods(x: a.ValueNode)(implicit c: Ctx) = processVal(x)
  def tnods(x: a.TypeNode)(implicit c: Ctx) = processTyp(x)

  // TODO detect forwards non-function value refs? -- or actually allow them
//  def vars(x: a.VarSym) =
//    if (state.varTable isDefinedAt x.get.uid) apply(x.get)
//    else throw CompileError(s"Illegal forward reference of local variable ${x.get.nam}")
  
}






























