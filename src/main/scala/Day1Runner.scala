import scala.io.Source

object Day1Runner {

  def main(args: Array[String]) = {
    for (line <- Source.fromFile("src/main/resources/input/day1").getLines()) {
      println(processCode(line))
    }
  }

  class AccHelper(xacc: Int, xlast: Char) {
    val acc: Int = xacc
    val last: Char = xlast
  }

  def processCode(in: String): String = {
    in.foldRight(new AccHelper(0, 'x'))((nextChar: Char, acc: AccHelper) => {
      if (nextChar == acc.last) {
        new AccHelper(acc.acc + Integer.parseInt(acc.last.toString(), 10), nextChar)
      } else {
        new AccHelper(acc.acc, nextChar)
      }
    }).acc.toString()
  }
}