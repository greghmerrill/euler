package euler0020

import euler.EulerSolution

object Euler0017 extends EulerSolution {

  override def expect = 21124
  
  def solve = {
    val words1to9 = List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
    val words10to19 = List("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")

    val _20to90 = List("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
    val words20to99 = _20to90.foldLeft(List[String]()) { (words, str) =>
      words ::: List(str) ::: words1to9.map(n => str + n)
    }
    
    val words100to999 = words1to9.foldLeft(List[String]()) { (words, str) =>
      val thisHundred = str + "hundred"
      words ::: List(thisHundred) ::: 
        words1to9.map(n => thisHundred + "and" + n) :::
        words10to19.map(n => thisHundred + "and" + n) :::
        words20to99.map(n => thisHundred + "and" + n)
    }
    
    val words = words1to9 ::: words10to19 ::: words20to99 ::: words100to999 ::: List("onethousand")    
    words.foldLeft(0) { (sum, word) => sum + word.length }
  }

}
