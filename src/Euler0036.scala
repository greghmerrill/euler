object Euler0036 extends EulerSolution {

  def isPalindrome(s: String) = s.equals(s.toString.reverse)

  def solve = 1.to(999999, 2) filter { x => isPalindrome(x.toString) && isPalindrome(Integer.toString(x, 2)) } sum

}
