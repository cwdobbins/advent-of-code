import scala.io.Source

object Day1Runner {

  def main(args: Array[String]) = {
    for (line <- Source.fromFile("src/main/resources/day1/input").getLines()) {
      println(processCode(line))
    }

//    println(processCode("1122"))
//    println(processCode("1111"))
//    println(processCode("1234"))
//    println(processCode("912121299"))
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