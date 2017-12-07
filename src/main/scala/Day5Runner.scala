import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day5Runner {

  def main(args: Array[String]) = {
    assert(executeInstructions(Array(0, 3, 0, 1, -3)) (_ => 1) == 5)
    assert(executeInstructions(Array(0, 3, 0, 1, -3)) (jump => if (jump >= 3) -1 else 1 ) == 10)

    //Part 1
    println(executeInstructions(
      buildJumpMap("src/main/resources/input/day5"))
      (_ => 1)
    )

    //Part 2
    println(executeInstructions(
      buildJumpMap("src/main/resources/input/day5"))
      (jump => if (jump >= 3) -1 else 1 )
    )
  }

  def buildJumpMap(filename: String): Array[Int] = {
      (for {
        line <- Source.fromFile(filename).getLines()
      } yield Integer.parseInt(line, 10)).toArray
  }

  def executeInstructions(jumpMap: Array[Int])(modifier:Int => Int): Int = {
    @tailrec
    def executeInstruction(index: Int, counter: Int): Int = {
      val instruction = jumpMap(index)
      if (index + instruction >= jumpMap.length || index + instruction < 0)
        return counter
      else
        jumpMap(index) = jumpMap(index) + modifier(instruction)
        return executeInstruction(index + instruction, counter + 1)
    }
    executeInstruction(0, 1)
  }
}
