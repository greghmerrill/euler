import euler.EulerSolution
import scala.math.sqrt

object Euler0039 extends EulerSolution {

  override def expect = 840
  
  def solve = {
    
    val maxPerimeter = 1000

    def rightTrianglePerimeters(a: Int): List[Int] = {
      val triangles = (a + 1).to((maxPerimeter / 2 - 1)) map { b =>
        val c = sqrt(a * a + b * b).toInt
        val perim = a + b + c
        if (a * a + b * b == c * c && perim < maxPerimeter) Option(perim)
        else None
      }
      triangles filter { !_.isEmpty } map { t => t.get } toList
    }

    val perims = 1.to(maxPerimeter / 3) map(rightTrianglePerimeters(_)) flatten;
    perims groupBy(x => x) map(kv => kv._1 -> kv._2.size) maxBy(kv => kv._2) _1
  }

}
