import java.util.Scanner

import scala.io.Source

object Day2Runner {

  def main(args: Array[String]) = {
    println((for (line <- Source.fromFile("src/main/resources/input/day2").getLines())
      yield minMaxDiffLine(line)).reduce {_ + _})
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
}