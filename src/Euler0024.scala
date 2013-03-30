object Euler0024 extends EulerSolution {

  override def expect = 2783915460L

  def fact(n: Int): Int = if (n == 1) n else n * fact(n - 1)

  def solve = {
    def findPerm(termPos: Int, remainingTerms: List[Int], remainder: Int, perm: List[Int] = List()): List[Int] = {
      if (termPos == 0) remainingTerms.head :: perm
      else {
        val termFact = fact(termPos)
        val index = remainder / termFact
        val term = remainingTerms(index)
        findPerm(termPos - 1, remainingTerms.filterNot(_ == term), remainder - (termFact * index), term :: perm)
      }
    }
    findPerm(9, (0 to 9).toList, 999999).reverse.mkString("").toLong
  }

}
