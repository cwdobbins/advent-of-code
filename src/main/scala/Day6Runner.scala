import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.collection.parallel.mutable
import scala.io.Source

object Day6Runner {

  def main(args: Array[String]) = {
    println(rebalanceBlocks(Array(0, 2, 7, 0)))
    assert(rebalanceBlocks(Array(0, 2, 7, 0)) == 4)

    //Part 1
    for (line <- Source.fromFile("src/main/resources/input/day6").getLines()) {
      println(rebalanceBlocks(buildBlockMap(line)))
    }

    //Part 2
    //println(rebalanceBlocks(
    //  buildBlockMap("src/main/resources/input/day6"))
    //)
  }

  def buildBlockMap(line: String): Array[Int] = {
      line.split("\\s").map(field => Integer.parseInt(field))
  }

  def rebalanceBlocks(blockMap: Array[Int]): Int = {
    var permutationList = List.empty[String]

    def findMaxIndex(blockMap: Array[Int]): Int = blockMap.indexOf(blockMap.max)

    def toValue(blockMap: Array[Int]): String = {
      blockMap.foldLeft("") {(acc, next) => acc + next.toString}
    }

    @tailrec
    def redistributeBlocks(blockCount: Int, index: Int): String = {
      if (blockCount == 0)
        toValue(blockMap)
      else {
        val nextIndex = if (index >= blockMap.length - 1) 0 else index + 1
        blockMap(index) = blockMap(index) + 1
        redistributeBlocks(blockCount - 1, nextIndex)
      }
    }

    @tailrec
    def recursiveRebalance(count: Int): Int = {
      val blockCount = blockMap.max
      val index = findMaxIndex(blockMap)
      blockMap(index) = 0
      val nextPermutation = redistributeBlocks(blockCount, if (index + 1 >= blockMap.length) 0 else index + 1)

      if (permutationList.contains(nextPermutation))
        //count + 1
        count + 1 - permutationList.reverse.indexOf(nextPermutation) - 1
      else {
        permutationList = nextPermutation :: permutationList
        recursiveRebalance(count + 1)
      }
    }

    recursiveRebalance(0)
  }
}
