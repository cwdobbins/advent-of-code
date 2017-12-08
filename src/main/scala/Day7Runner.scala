import scala.annotation.tailrec
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers


object Day7Runner {

  def main(args: Array[String]) = {
    //println(InputLexer("qsnjuqh (34) -> asdfajs, ajskjokj, asdjfoas"))
    //Part 1
    val setup = readInput(Source.fromFile("src/main/resources/input/day7").getLines())
  }

  object InputLexer extends RegexParsers {
    sealed trait ParserToken
    case class IDENTIFIER(str: String) extends ParserToken
    case class WEIGHT(str: String) extends ParserToken
    case object ARROW extends ParserToken
    case object COMMA extends ParserToken

    override def skipWhitespace = true

    def identifier: Parser[IDENTIFIER] = {
      "[a-zA-Z]+".r ^^ {str => IDENTIFIER(str)}
    }
    def weight: Parser[WEIGHT] = {
      "\\([0-9]+\\)".r ^^ { str =>
        WEIGHT(str.substring(1, str.length - 1))
      }
    }
    def comma = ","    ^^ (_ => COMMA)
    def arrow = "->"   ^^ (_ => ARROW)

    def tokens: Parser[List[ParserToken]] = {
      phrase(rep(identifier | weight | arrow | comma))
    }

    trait ParserCompilationError
    case class ParserLexerError(msg: String) extends ParserCompilationError

    def apply(code: String): List[ParserToken] = {
      parse(tokens, code) match {
        case Success(result, next) => result
      }
    }
  }


  def readInput(lines: Iterator[String]): Unit = {
    for (line <- lines) {
      println(InputLexer(line))
    }
  }
}
