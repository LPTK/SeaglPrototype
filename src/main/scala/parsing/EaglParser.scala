package parsing

import scala.util.parsing.combinator._

class EaglParser extends RegexParsers {

  def word = """[a-zA-Z]""".r ^^ { _.toString }

}
