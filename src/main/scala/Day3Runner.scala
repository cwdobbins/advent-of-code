import scala.annotation.tailrec

object Day3Runner {

  def main(args: Array[String]) = {
//    assert(calculateManhattanDist(1) == 0)
//    assert(calculateManhattanDist(3) == 2)
//    assert(calculateManhattanDist(2) == 1)
//    assert(calculateManhattanDist(12) == 3)
//    assert(calculateManhattanDist(23) == 2)
//    assert(calculateManhattanDist(24) == 3)
//    assert(calculateManhattanDist(25) == 4)
//    assert(calculateManhattanDist(50) == 7)
//    assert(calculateManhattanDist(51) == 6)
//    assert(calculateManhattanDist(52) == 5)
//    assert(calculateManhattanDist(54) == 5)
//    assert(calculateManhattanDist(81) == 8)
//    assert(calculateManhattanDist(79) == 6)
//    assert(calculateManhattanDist(82) == 9)
//    assert(calculateManhattanDist(1024) == 31)
    println(calculateManhattanDist(277678))
  }

  def calculateManhattanDist(value: Int): Int = {
    if (value == 1) return 0

    var gridSize = Math.ceil(Math.sqrt(value)).toInt
    if (gridSize % 2 == 0) gridSize = gridSize + 1

    @tailrec
    def recursiveFindValue(index: Int, currentPath: Int, target: Int, min: Int, max: Int, next: Int): Int = {
      if (index == target)
        currentPath
      else if (currentPath == min) {
        recursiveFindValue(index + 1, currentPath + 1, target, min, max, 1)
      } else if (currentPath == max) {
        recursiveFindValue(index + 1, currentPath - 1, target, min, max, -1)
      } else {
        recursiveFindValue(index + 1, currentPath + next, target, min, max, next)
      }
    }

    recursiveFindValue(
      Math.pow(gridSize - 2, 2).toInt + 1,
      gridSize - 2,
      value,
      (gridSize - 1) / 2,
      gridSize - 1,
      -1
    )
  }
}