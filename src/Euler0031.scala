object Euler0031 extends EulerSolution {

  override def expect = 73682
  
  def solve = {
    val denoms = List(1, 2, 5, 10, 20, 50, 100, 200)
    
    def change(remainingDenoms: List[Int], remainingAmt: Int, combos: Int): Int = {
      if (remainingDenoms.isEmpty) combos
      else {
        val denom = remainingDenoms.head
        val combosForDenom =
          if (remainingAmt == denom) 1
          else if (remainingAmt >= denom) change(remainingDenoms, remainingAmt - denom, 0)
          else 0
        change(remainingDenoms.tail, remainingAmt, combos + combosForDenom)
      }
    }
    change(denoms, 200, 0)
  }

}
