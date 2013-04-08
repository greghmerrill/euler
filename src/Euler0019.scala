import euler.EulerSolution

object Euler0019 extends EulerSolution {

  def solve = {
    val thirtyDays = Set(3, 5, 8, 10) // April, June, September, November 
    def daysInMonth(month: Int, year: Int) = month match {
      case 1 =>
        if (year % 4 == 0 && year % 100 != 0) 29
        else if (year % 4 == 0 && year % 400 == 0) 29
        else 28
      case n if thirtyDays.contains(n) => 30
      case _ => 31
    }

    var day = 1 // Monday
    var sundays = 0
    for (year <- 1900 to 2000) {
      for (month <- 0 to 11) {
        if (day % 7 == 0 && year > 1900) sundays += 1
        day += daysInMonth(month, year)
      }
    }
    sundays
  }

}
