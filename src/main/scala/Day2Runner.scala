import java.util.Scanner

import scala.io.Source

object Day2Runner {

  def main(args: Array[String]) = {
    assert(minMaxDiffLine("1 3 5 7") == 6)
    assert(divisibleResultLine("5 9 2 8") == 4)

    //Part 1
    println((for (line <- Source.fromFile("src/main/resources/input/day2").getLines())
      yield minMaxDiffLine(line)).reduce {_ + _})

    //Part 2
    println((for (line <- Source.fromFile("src/main/resources/input/day2").getLines())
          yield divisibleResultLine(line)).reduce {_ + _})
  }

  def minMaxDiffLine(in: String): Int = {
    val scan = new Scanner(in)
    var min = Int.MaxValue
    var max = Int.MinValue
    while (scan.hasNext()) {
      val next = scan.nextInt()
      if (next < min) min = next
      if (next > max) max = next
    }
    Math.abs(max - min)
  }

  def divisibleResultLine(in: String): Int = {
    val map = in.split("\\s+").map {c => Integer.parseInt(c.toString(), 10)}.par
    val things = map.map(c => map.filter(d => c != d && c % d == 0).map(d => (c, d)))
                      .filter {_.nonEmpty}(0)(0)
    if (things._1 > things._2)
      things._1 / things._2
    else things._2 / things._1
  }
}