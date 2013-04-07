object Euler0037 extends EulerSolution {

  override def expect = 748317
  
  def isPrime(n: Int) = PrimeCache.set.contains(n)

  def trunc(prime: String, truncFn: String => String, depth: Int = 0): Boolean = {
    if (prime.size == 1 && isPrime(prime.toInt)) true
    else isPrime(prime.toInt) && trunc(truncFn(prime), truncFn, depth + 1)
  }

  def truncLeft(prime: String) = trunc(prime, p => p.slice(1, p.size))
  def truncRight(prime: String) = trunc(prime, p => p.slice(0, p.size - 1))

  def solve = PrimeCache.list filter { p => p > 7 && truncLeft(p.toString) && truncRight(p.toString) } sum

}

object PrimeCache {
  val list = Primes.sieve(1000000)
  val set = Set() ++ list
}
