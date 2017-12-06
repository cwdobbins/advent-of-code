import scala.io.Source

object Day1Runner {

  def main(args: Array[String]) = {
    assert(processCodePart2("123123") == 12)

    for (line <- Source.fromFile("src/main/resources/input/day1").getLines()) {
      println(processCodePart2(line))
    }
  }

  class AccHelper(xacc: Int, xlast: Char) {
    val acc: Int = xacc
    val last: Char = xlast
  }

  def processCode(in: String): Int = {
    in.foldRight(new AccHelper(0, 'x'))((nextChar: Char, acc: AccHelper) => {
      if (nextChar == acc.last) {
        new AccHelper(acc.acc + Integer.parseInt(acc.last.toString(), 10), nextChar)
      } else {
        new AccHelper(acc.acc, nextChar)
      }
    }).acc
  }

  def processCodePart2(in: String): Int  = {
    in.zip(in.substring(in.length / 2).concat(in.substring(0, in.length / 2)))
      .foldRight(0)((nextPair: (Char, Char), acc: Int) => {
        if (nextPair._1 == nextPair._2) {
          acc + Integer.parseInt(nextPair._1.toString(), 10)
        } else {
          acc
        }
      })
  }
}