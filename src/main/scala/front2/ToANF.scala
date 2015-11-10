package front2

import front2.SeparateTypes.tconv._
import utils._

import common._
import Stages2._

/**
  * TODO: properly convert patterns by reversing stmts order and let directions
  *   TODO: put extracted pattern stmts AFTER the let!!
  * TODO: before ANF, a phase that checks name resolution and lifts or-patterns into proper lambdas
  * 
  * TODO: make tests for this class!!
  * 
  */
object ToANF extends StageConverter[AST.type, Desugared.type](AST, Desugared) {
toanf =>
  
  val phaseName = "ToANF"
  
  /**
   * Stores the transformed object (in `res`) as well as the new statements that the transformation has generated (in `genStmts`)
   * Note: could be done with a State monad?
   * 
   * Values nodes generate new statements while extracting non-sub-terms from sub-term position (eg: "f a" in "f a b")
   * Both values and types may also generate statements when removing the ModBlock syntactic tree. 
   */
  case class Result[+T](genStmts: Ls[b.AnyStmt], res: T) {
    def toBlock(implicit ev: T <:< b.values.Node) =
      //b.values.Node(b.values.Block(genStmts, res), Synthetic("toplevel", None))
      vconv.mkBlock(genStmts: _*)(res) // TODO use md: Synthetic("toplevel", None))
  }
  implicit object Result extends Monad[Result] {
    def lift[A](a: A) = Result(Nil, a)
    def flatMap[A,B](ma: M[A], f: A => M[B]): M[B] = {
      val Result(newStmts, r) = f(ma.res)
      Result(ma.genStmts ++ newStmts, r)
    }
    def map[A,B](ma: F[A], f: A => B): F[B] = Result(ma.genStmts, f(ma.res))
  }
  import Result._
  
  /** `tstmt` and `vstmt` are never supposed to be called -- `process` will deal with statements directly */
//  def tstmt(x: a.TypeStmt): Result[b.TypeStmt] = wtf
//  def vstmt(x: a.ValueStmt): Result[b.ValueStmt] = wtf
  
  /**
    * Expects that in AST, ModBlocks have the modifier Type if it contains type statements
    * Also, for now let statements in AST are wrapped as expressions in Impure statements... (don't ask)
    */
  override def process(x: a.AnyStmt): Result[b.AnyStmt] = {
    val stmt = x.fold (identity, identity)
    stmt match {
      case a.ModBlock(modifs, stmts) =>
        val sts2 = stmts map process flatMap { case Result(s,r) => s :+ r }
        if (modifs contains Rec)
          if (modifs contains Type) b.types.RecBlock(sts2) |> b.t2stmt |> lift
          else b.values.RecBlock(sts2) |> b.v2stmt |> lift
        else Result(sts2.init, sts2.last)
      //case ta.Impure(ta.Node(let: ta.Let)) => Left(vconv.process(let))
      case a.values.Impure(a.values.Node(let: a.values.Let)) =>
        //vconv.process(let) map Right.apply
        vconv.let(let) match {
          case Result(pre, let -> Nil) => Result(pre, Right(let))
          case Result(pre, let -> (i :+ l)) => Result(pre ++ (Right(let) :: i), l)
        } //map Right.apply
      case a.values.Impure(n) => vconv.nod(n) map b.values.Impure map Right.apply
      // DEAD:
      //case x: a.types.Stmt => tconv.stmt(x) map Left.apply
      //case x: a.values.Stmt => vconv.stmt(x) map Right.apply
    }
  }
  
  def mod(x: a.Modif): Result[b.Modif] = Modification(x contains Priv) |> lift
  
  
  //val tconv: AnfTermsConverter[AST.types.type,Desugared.types.type] = new AnfTermsConverter(AST.types, Desugared.types) with TypeConverter {
  //object tconv extends AnfTermsConverter[AST.types.type,Desugared.types.type] with TypeConverter {
  //  val (ta,tb) = (AST.types, Desugared.types)
  object tconv extends AnfTermsConverter(AST.types, Desugared.types) with TypeConverter {
    //val co: vconv.type = vconv
    
    //def blockAsTerm = ??? //identity
    
    def kin(x: ta.Kind): Result[tb.Kind] = x |> lift
    
    
    /** For types, we know nodes will never generate additional statements (types.App is not moved out of subexpressions) */
    def nod(x: ta.Node): Result[tb.Node] = tb.Node(ast2Core(x.term).res, x.md) |> lift
    
    //def snod(x: ta.SubNode): Result[tb.SubNode] = b.TypeNode(ast2Core(x.term).res, x.md) |> lift
    def snod(x: ta.SubNode): Result[tb.SubNode] = nod(x)
    
//    def ast2Core(x: ta.ASTTerm): Result[tb.CoreTerm] = //???
//      (x match {
//        case ta.Lambda(id: Ident, bo) => tb.Closure(id, nod(bo).res)
//        case ta.Lambda(pa, bo) =>
//          val id = new SyntheticId(nameHint(pa.term))
//          val idn = tb.Node(tb.Id(id): tb.Term, Synthetic(phaseName, pa.md))
//          val let = tb.Let(Modification(false), nod(pa).res, idn)
//          val bo2 = nod(bo).res
//          tb.Closure(id, mkBlock(let)(bo2)) // TODO the right thing (closure aggregates stmts) //{b.TypeNode(_, bo2.md)})
//  //      case ta.OpAppL(ar, op) => tb.App(tb.Node(tb.Id(op.id), x.md/*TODO better md*/), nod(ar).res)
//  //      case ta.OpAppR(op, ar) => tb.App(tb.Node(tb.Id(op.id), x.md/*TODO better md*/), nod(ar).res) // tb.App(nod(ta.OpTerm(op)), nod(ar).res)
//  //      case ta.OpTerm(op) => tb.Id(op.id)
////        case st: ta.SubTerm => process(st).res
////        case gt: ta.GenTerm => process(gt).res
//        case _ => ??? // TODO
//      }) |> lift
    
//    def mkBlock(stmts: b.AnyStmt*)(ret: tb.Node) = tb.Node(ret.term match {
//      case tb.Block(s, r) => tb.Block(stmts ++ s toList, r) |> blockAsTerm
//      case _ => tb.Block(stmts toList, ret) |> blockAsTerm
//    }, ret.md) // TODO // MixedOrg((stmts map (_ org)) :+ ret.org))
    
  }
  
  
  //val vconv = new AnfTermsConverter(AST.values, Desugared.values) with ValueConverter {
  //object vconv extends AnfTermsConverter[AST.values.type,Desugared.values.type] with ValueConverter {
  //  val (ta,tb) = (AST.values, Desugared.values)
  object vconv extends AnfTermsConverter(AST.values, Desugared.values) with ValueConverter {
    //val co: tconv.type = tconv
    
    //def blockAsTerm = ??? //identity
    
    def kin(x: ta.Kind): Result[tb.Kind] = tconv.ast2Core(x)
    
    def nod(x: ta.Node): Result[tb.Node] = ast2Core(x.term) map {tb.Node(_, x.md)}
    
    /** For values, a simple node may generate several new statements */
    def snod(x: ta.SubNode): Result[tb.SubNode] = {
      val Result(sts, ret) = ast2Core(x.term)
      val (s,t) = ret match {
        case ret: tb.SubTerm => (Nil, ret)
        case _ =>
          val id = new SyntheticId(nameHint(x.term))
          val idt = tb.Id(id)
          val idn = tb.Node(idt, Synthetic(phaseName, Some(x.md)))
          val let = tb.Let(Modification(false), idn, tb.Node(ret, x.md)) |> b.v2stmt //: b.AnyStmt
          (let :: Nil, idt)
      }
      Result(sts ++ s, new tb.SubNode(t, x.md))
    }
    
//    
//    /** TODO: that's almost the same as in tconv :( ... would be nice to refactor */
//    def mkBlock(stmts: b.AnyStmt*)(ret: tb.Node) = tb.Node(ret.term match {
//      case tb.Block(s, r) => tb.Block(stmts ++ s toList, r) |> blockAsTerm
//      case _ => tb.Block(stmts toList, ret) |> blockAsTerm
//    }, ret.md) // TODO // MixedOrg((stmts map (_ org)) :+ ret.org))
//    
  }
  
  
  //abstract class AnfTermsConverter[TA <: a.TermsTemplate, TB <: b.TermsTemplate { type Term = TB# CoreTerm }](val _ta: TA, val _tb: TB)
  abstract class AnfTermsConverter[TA <: a.AST, TB <: b.Core](val ta: TA, val tb: TB) extends TermsConverter[TA,TB] { //(_ta,_tb) {
  //trait AnfTermsConverter[TA <: a.AST, TB <: b.Core] extends TermsConverter[TA,TB] { //(_ta,_tb) {
    
    val LambdaCompoId = StableId('std::Nil, Sym("&>"))
    
    //def blockAsTerm: tb.Block => tb.Term
    
    def nameHint(x: ta.Term): ?[Sym] = ?(x match {
      case ta.Id(SyntheticId(Some(sym))) => sym
      case ta.Id(StableId(_, sym)) => sym
      case ta.Id(LocalId(sym)) => sym
      //case ta.App(_,_) | ta.DepApp(_,_) => 'app
      case ta.App(a,b) => (for{Sym(a) <- nameHint(a.term); Sym(b) <- nameHint(b.term)} yield Sym(a+b)) getOrElse 'app
      case ta.DepApp(_,_) => 'dapp
      //case _: ta.Block => 'blk
      case ta.Block(_, r) => nameHint(r.term) getOrElse 'blk
      case _ => null
    })
    
    //def ast2Core(x: ta.ASTTerm): Result[tb.CoreTerm]
    
    /** `types.stmt` and `values.stmt` are never supposed to be called -- `toanf.process` will deal with statements directly */
    def stmt(x: ta.Stmt): Result[tb.Stmt] = wtf 
//    x match {
//      case x: ta.ComStmt => process(x)
//      case x: ta.ASTStmt => ???
//    }
    
    //def ast2Core(x: ta.ASTTerm): Result[tb.CoreTerm] = ???
    //def ast2Gen(x: ta.ASTTerm): Result[tb.GenTerm] = ???
    //def ast2Core(x: ta.ASTTerm): Result[tb.CoreTerm] = ???
//      x match {
//      case ta.OpAppL(ar, op) => tb.App(tb.Node(tb.Id(op.id), x.md/*TODO better md*/), nod(ar).res)
//      case ta.OpAppR(op, ar) => tb.App(tb.Node(tb.Id(op.id), x.md/*TODO better md*/), nod(ar).res) // tb.App(nod(ta.OpTerm(op)), nod(ar).res)
//      case ta.OpTerm(op) => tb.Id(op.id)
//    }
    
//    def mkBlock(stmts: b.AnyStmt*)(ret: tb.Node)(node: tb.Block => tb.Node) = node(ret.term match {
//      case tb.Block(s, r) => tb.Block(stmts ++ s toList, r) |> blockAsTerm
//      case _ => tb.Block(stmts toList, ret) |> blockAsTerm
//    }) //, ret.md // TODO // MixedOrg((stmts map (_ org)) :+ ret.org))
    
    /** TODO: that's almost the same as in tconv :( ... would be nice to refactor */
    def mkBlock(stmts: b.AnyStmt*)(ret: tb.Node) = tb.Node(ret.term match {
      case tb.Block(s, r) => tb.Block(stmts ++ s toList, r) //|> blockAsTerm
      case _ => tb.Block(stmts toList, ret) //|> blockAsTerm
    }, ret.md) // TODO // MixedOrg((stmts map (_ org)) :+ ret.org))
   
    def ast2Core(x: ta.ASTTerm): Result[tb.CoreTerm] = x match {
      case ta.Block(sts, ret) =>
        val Result(pre, sts2) = Monad.sequence(sts map toanf.process)
        //nod(ret) map {tb.Block(pre ++ sts2, _)}
        val Result(rsts, r) = nod(ret)
        tb.Block(pre ++ sts2 ++ rsts, r) |> lift
        //???
      case x: ta.ComTerm => process(x)
      case x: ta.Let =>
        //process(x: ta.ComStmt) flatMap {s => Result(tb.stmt2anyS(s) :: Nil, tb.Literal(()))}
        // eqtly
        //val Result(sts, res) = process(x: ta.ComStmt); Result(sts :+ tb.stmt2anyS(res), () |> tb.Literal.apply)
        val Result(sts, (res, post)) = let(x)
        Result((sts :+ tb.stmt2anyS(res)) ++ post, () |> tb.Literal.apply)
      //case ta.Lambda(id: Ident, bo) => //nod(bo) map {tb.Closure(id, _)}
      case ta.Lambda(ta.Node(ta.Id(id)), bo) =>
        //val Result(sts, ret) = nod(bo)
        (nod(bo) match {
          case Result(Nil, r) => tb.Closure(id, r)
          case Result(sts, r) => tb.Closure(id, mkBlock(sts: _*)(r))
        }) |> lift
      case ta.Lambda(pa, bo) =>
        // FIXME: what to do with the statements introduced by the pattern??
        val id = new SyntheticId(Some('arg)) //(nameHint(pa.term))
        val idn = tb.Node(tb.Id(id): tb.Term, Synthetic(phaseName, Some(pa.md)))
        //for {
        //  pa <- invert(nod(pa))
        //  let = tb.Let(Modification(false), pa, idn) |> tb.stmt2anyS //: b.AnyStmt
        //  //bo <- nod(bo)
        //  Result(sts, bo2) = nod(bo)
        //} yield tb.Closure(id, mkBlock(let :: sts : _*)(bo2))
        val Result(post, pa2) = invert(nod(pa))
        val let = tb.Let(Modification(false), pa2, idn) |> tb.stmt2anyS
        val Result(sts, bo2) = nod(bo)
        tb.Closure(id, mkBlock(let :: (sts ++ post) : _*)(bo2)) |> lift
      case ta.LambdaCompo(lams) =>
//        val md = Synthetic(phaseName, None)
//        for (lams <- Monad.sequence(lams map ast2Core))
//          yield lams.map{case x: tb.Closure => tb.SubNode(x, md) case _ => wtf}.reduce(tb.App.apply)
        // Actually, reduce to Apps BEFORE converting!
        val Nd = ta.Node
        val apps = lams map Nd reduce {(a,b) => ta.App(ta.App(a, LambdaCompoId |> ta.Id |> Nd) |> Nd, b) |> Nd}
        ast2Core(apps.term)
      case ta.OpAppL(ar, op) => snod(ar) map {tb.App(_, tb.SubNode(tb.Id(op.id), Synthetic(phaseName, None)/*TODO better md*/))}
      case ta.OpAppR(op, ar) => snod(ar) map {tb.App(tb.SubNode(tb.Id(op.id), Synthetic(phaseName, None)/*TODO better md*/), _)} // tb.App(nod(ta.OpTerm(op)), nod(ar).res)
      case ta.OpTerm(op) => tb.Id(op.id) |> lift
      //case _ => ??? // TODO OpApp, Let, etc.
    }
    
//    override def process(x: ta.ComStmt) = x match {
//      case x: ta.Let => 
//      case _ => super.process(x)
//    }
    
////    override def process(x: tb.Let) = super.process(x) match {
////      case tb.Let(mo, pa, bo, wh) => tb.Let(mo, invert(pa), bo, wh)
//    override def process(x: ta.Let) = x match {
//      case ta.Let(mo, pa, bo, wh) => for {
//        mo <- mod(mo)
//        pa <- invert(nod(pa))
//        bo <- nod(bo)
//        wh <- Monad.sequence(wh map toanf.process)
//      } yield tb.Let(mo, pa, bo, wh)
//    }
    def let(x: ta.Let): Result[tb.Let * Ls[b.AnyStmt]] = x match {
      case ta.Let(mo, pa, bo, wh) => for {
        mo <- mod(mo)
        Result(post, pa2) = invert(nod(pa))
        bo <- nod(bo)
        wh <- Monad.sequence(wh map toanf.process)
      } yield tb.Let(mo, pa2, bo, wh) -> post
    }
    
    def invert[T](x: Result[T]): Result[T] = x match {
      case Result(genStmts, res) => Result(genStmts map reverse reverse, res)
    }
    
  }
  def reverse(x: b.AnyStmt): b.AnyStmt = x match {
    case Left(b.types.Let(mo, pa, bo, wh)) => Left(b.types.Let(mo, bo, pa, wh))
    case Right(b.values.Let(mo, pa, bo, wh)) => Right(b.values.Let(mo, bo, pa, wh))
    case _ => x
  }
  
}














