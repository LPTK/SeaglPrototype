package front2.dsl

import front2._ //SeparateTypes
import parsing.Parser
import utils._
import common._
import Stages2.Desugared._

/**
  * TODO factor pipeline with REPL
  * 
  */
class ANFContext(ctx: StringContext) {
  import values._  

  object anf {
    
    def apply(nodes: Node*): Node = {
      val code = (ctx.parts.init map { p => p + "(__hole)" }) :+ ctx.parts.last mkString
      val pgrm = Parser.pgrm(new Parser.lexical.Scanner(code))  
      val septyps = SeparateTypes.vconv.nod(pgrm getOrElse {
        throw CompileError(pgrm toString)
      })
      val anf = ToANF.vconv.nod(septyps).toBlock
      anf
    }
    
  }
  
}



