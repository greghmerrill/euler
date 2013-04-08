import euler.EulerSolution

object Euler0014 extends EulerSolution {

  override def expect = 837799
  
  def calcCollatzTerms(n: Long, terms: List[Long]): List[Long] = n match {
    case 1 => 1 :: terms
    case n if n % 2 == 0 => calcCollatzTerms(n / 2, n :: terms)
    case _ => calcCollatzTerms((3 * n) + 1, n :: terms)
  }
  def collatzTerms(n: Long) = calcCollatzTerms(n, List()).reverse

  def solve = {
    def mostTerms(n: Int, maxN: Int, maxTerms: Int): Int = n match {
      case 1000000 => maxN
      case _ => {
        val collatzTermCount = collatzTerms(n).length
        if (collatzTermCount > maxTerms) mostTerms(n + 1, n, collatzTermCount)
        else mostTerms(n + 1, maxN, maxTerms)
      }
    }
    mostTerms(1, 0, 0)
  }

}
