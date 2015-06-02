package parsing

import scala.util.parsing.combinator._

class SeaglParser extends RegexParsers {

  def word = """[a-zA-Z]""".r ^^ { _.toString }

}
