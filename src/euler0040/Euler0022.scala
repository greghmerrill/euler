package euler0040

import euler.EulerSolution
import scala.io.Source

object Euler0022 extends EulerSolution {

  override def expect = 871198282

  def solve = {
    val allNames = Source.fromFile("src/euler0040/Euler0022.names.txt").getLines().next()
    val names = allNames.split(",").toList.map(_.replaceAll("\"", "")).sortWith((a, b) => a.compareTo(b) < 0)

    def sumOfScores(remainingNames: List[String], index: Int, sum: Int): Int = remainingNames match {
      case Nil => sum
      case _ => {
        val newSum = sum + remainingNames.head.map(c => c.toInt - 'A'.toInt + 1).sum * index
        sumOfScores(remainingNames.tail, index + 1, newSum)
      }
    }

    sumOfScores(names, 1, 0)
  }

}
