import scala.math.BigInt

object Euler0025 extends EulerSolution {

  override def expect = 4782

  def fib(upTo: BigInt): BigInt = {

    def fibIter(nm2: BigInt, nm1: BigInt, term: Int): BigInt = {
      val fib = nm2 + nm1
      if (fib / upTo >= 1) term
      else fibIter(nm1, fib, term + 1)
    }

    fibIter(1, 1, 3)
  }

  def solve = fib(BigInt(10).pow(999))

}
