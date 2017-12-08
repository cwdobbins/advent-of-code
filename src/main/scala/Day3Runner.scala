import scala.annotation.tailrec

object Day3Runner {

  def main(args: Array[String]) = {
    assert(calculateManhattanDist(79) == 6)
    assert(calculateManhattanDist(82) == 9)
    assert(calculateManhattanDist(1024) == 31)

    //Part 1
    println(calculateManhattanDist(277678))

    //Part 2
    println(computeValueAfter(277678))
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

  type Pos = (Int, Int)

  sealed trait Direction {
    def x: Int
    def y: Int
  }

  case object UPRIGHT extends Direction {val x = 1; val y = -1}
  case object RIGHT extends Direction {val x = 1; val y = 0}
  case object UPLEFT extends Direction {val x = -1; val y = -1}
  case object LEFT extends Direction {val x = -1; val y = 0}
  case object DOWNRIGHT extends Direction {val x = 1; val y = 1}
  case object UP extends Direction {val x = 0; val y = -1}
  case object DOWNLEFT extends Direction {val x = -1; val y = 1}
  case object DOWN extends Direction {val x = 0; val y = 1}

  type Cursor = (Pos, Direction, Set[Pos])

  def add(pos1: Pos, direction: Direction): Pos = (pos1._1 + direction.x, pos1._2 + direction.y)

  def next(cursor: Cursor): Cursor = {
    val (position, direction, set) = cursor
    val turn = direction match {
      case DOWN => RIGHT
      case UP => LEFT
      case LEFT => DOWN
      case RIGHT => UP
    }

    val nextTurn = add(position, turn)
    val nextStraight = add(position, direction)
    if (set.contains(nextTurn))
      (nextStraight, direction, set + nextStraight)
    else
      (nextTurn, turn, set + nextTurn)
  }

  def computeValueAfter(in: Int) = {
    val initial = ((0, 0), UP, Set((0, 0)))

    lazy val VALUES: Stream[(Cursor, Map[Pos, Int])] =
      Stream.cons((initial, Map((0, 0) -> 1)), VALUES.map { e =>
        val (cursor, map) = e
        val nextValue = next(cursor)
        (nextValue, map + (nextValue._1 ->
          (for {
            dir <- List(UP, RIGHT, LEFT, DOWN, UPRIGHT, UPLEFT, DOWNRIGHT, DOWNLEFT)
          } yield map.getOrElse(add(nextValue._1, dir), 0)).sum
        ))
      })

    VALUES.find {
      case ((position, _, _), map) => map.getOrElse(position, 0) >= in
    } match {
      case Some(e) => e._2(e._1._1).toString
    }
  }
}

