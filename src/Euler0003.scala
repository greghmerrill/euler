object Euler0003 extends EulerSolution {

  override def expect = 6857

  def isPrime(n: Long): Boolean = n match {
    case 2 => true
    case n if n % 2 == 0 => false
    case _ => {
      var i = 3
      while (i <= n / 2) {
        if (n % i == 0) return false
        i += 2
      }
      true
    }
  }

  def solve: Long = {
    val target = 600851475143L
    var i = 2
    while (true) {
      if (target % i == 0 && isPrime((target / i).toLong))
        return (target / i).toLong
      i += 1
    }
    throw new Exception("No prime factor for " + target)
  }

}
