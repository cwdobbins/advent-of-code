import java.util

import scala.collection.mutable
import scala.io.Source

object Day4Runner {

  def main(args: Array[String]) = {
    assert(checkPassPhrase("aa bb cc dd ee")(_ == _))
    assert(!checkPassPhrase("aa bb cc dd aa")(_ == _))

    //Part 1
    println((for {
      line <- Source.fromFile("src/main/resources/input/day4").getLines()
      if checkPassPhrase(line)(_ == _)
    } yield line).length)

    //Part 2
    println((for {
      line <- Source.fromFile("src/main/resources/input/day4").getLines()
      if checkPassPhrase(line)(charCount(_) == charCount(_))
    } yield line).length)
  }

  def checkPassPhrase(passPhrase: String)(f: (String, String) => Boolean): Boolean = {
    val phraseWords = passPhrase.split(" +").par
    phraseWords.map(word =>
      phraseWords.foldLeft((word, 0)) {
        (acc, other) =>
          if (f(other, word))
            (acc._1, acc._2 + 1)
          else acc
      }._2
    ).forall(result => result == 1)
  }

  def charCount(word: String): Map[Char, Int] = {
    val charMap = new mutable.HashMap[Char, Int]
    word.foreach {c => charMap.put(c, word.count(_ == c))}
    charMap.toMap
  }
}
