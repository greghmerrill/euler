package euler0060

import euler.EulerSolution

object Euler0043 extends EulerSolution {

  override def expect = 16695334890L

  def hasDupes(s: String) = s.toSet.size != s.size
  def paddedStr(i: Int) = "%03d" format i

  def pandigitals(primes: List[Int], candidates: List[String]): List[String] = primes match {
    case Nil => candidates map { c =>
      val diff = "0123456789".toSet -- c.toSet
      diff.toList.head + c
    }
    case _ => {
      val p = primes.head
      val start = (1.0 / p).ceil.toInt
      val end = (999.0 / p).floor.toInt
      val multiples = (start to end).map(m => paddedStr(p * m)).filter(m => !hasDupes(m)).toList
      val newCandidates: List[String] =
        if (candidates.isEmpty) multiples
        else {
          val concats =
            for (c <- candidates)
            yield multiples.filter(m => m.substring(1, 3) == c.substring(0, 2)).map(m => m.substring(0, 1) + c).filter(!hasDupes(_))
          concats.flatten
        }
      pandigitals(primes.tail, newCandidates)
    }
  }

  def solve = pandigitals(List(17, 13, 11, 7, 5, 3, 2), Nil).map(_.toLong).sum

}
