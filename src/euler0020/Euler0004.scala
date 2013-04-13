package euler0020

import euler.EulerSolution

object Euler0004 extends EulerSolution {

	override def expect = 906609

	def solve = {
		def isPalindrome(x: Int) = x.toString.equals(x.toString.reverse)
		val combos = (100 to 999) flatMap { x => (x to 999) map { y => x * y } }
		combos.filter(x => isPalindrome(x)).max
	}

}
