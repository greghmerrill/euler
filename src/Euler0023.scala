object Euler0023 extends EulerSolution {

  override def expect = 4179871

  def divisors(n: Int) = (1 to n / 2).filter { next => n % next == 0 }

  def solve = {
    println("Calculating abundants")
    val abundants = (1 to 28123).filter(n => divisors(n).sum > n).toList

    println("Calculating sums")
    def calcSums(remainingAbundants: List[Int], sums: Set[Int]): Set[Int] = remainingAbundants match {
      case Nil => sums
      case _ => calcSums(remainingAbundants.tail, sums ++ remainingAbundants.map(n => remainingAbundants.head + n))
    }
    val unsummable = (1 to 28123).toSet &~ calcSums(abundants, Set())
    unsummable.sum
  }

}
