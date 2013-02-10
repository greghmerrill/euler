object Euler0001 extends EulerSolution {
  def summation(n: Int) = ((n * n) + n) / 2
  def sumOfMultiples(n: Int) = n * summation(999/n)
  def solve = sumOfMultiples(3) + sumOfMultiples(5) - sumOfMultiples(15) 
}
