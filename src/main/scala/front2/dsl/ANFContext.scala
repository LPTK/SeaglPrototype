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
      val code = ctx.parts map { p => p + "__hole" } mkString
      val pgrm = Parser.pgrm(new Parser.lexical.Scanner(code)).get // TODO handle error  
      val septyps = SeparateTypes.vconv.nod(pgrm)
      val anf = ToANF.vconv.nod(septyps).toBlock
      anf
    }
    
  }
  
}



