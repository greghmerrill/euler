object Euler0002 extends EulerSolution {
  def fibs(upTo: Int): List[Int] = {
    var fibs = List(2, 1)
    while (fibs.head < upTo) {
      fibs = fibs.head + fibs.tail.head :: fibs
    }
    fibs.tail.reverse
  }
	def solve = {
	  fibs(4000000) filter(_ % 2 == 0) reduceLeft((a, b) => a + b)
	}
}