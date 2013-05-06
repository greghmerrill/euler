package euler0060

import euler.EulerSolution
import scala.io.Source

object Euler0042 extends EulerSolution {

  def triangle(n: Int) = ((0.5 * n) * (n + 1)).toInt
  
  def solve = {
    val allWords = Source.fromFile("src/euler0060/words.txt").getLines().next()
    val words = allWords.split(",").toList.map(_.replaceAll("\"", ""))
    val wordVals = words map { word => 
      (word map { char => char.toInt - 'A'.toInt + 1 }).toList.sum
    }
    
    val triangles = (for (n <- 1.to(wordVals.max)) yield triangle(n))
    val triangleWords = wordVals filter (triangles.contains(_))
    triangleWords.size
  }

}
