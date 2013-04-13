package euler0020

import euler.EulerSolution

object Euler0002 extends EulerSolution {

  override def expect = 4613732

  def fibs(upTo: Int, fibList: List[Int] = List(2, 1)): List[Int] = {
    if (fibList.head > upTo) fibList.tail
    else fibs(upTo, fibList.head + fibList.tail.head :: fibList)
  }

  def solve = fibs(4000000).filter(_ % 2 == 0).sum

}
