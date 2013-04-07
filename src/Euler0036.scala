object Euler0036 extends EulerSolution {

  def isPal(s: String) = s.equals(s.toString.reverse)

  def solve = 1.to(999999, 2) filter { x => isPal(x.toString) && isPal(Integer.toString(x, 2)) } sum

}
