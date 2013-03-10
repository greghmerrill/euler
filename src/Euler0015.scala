object Euler0015 extends EulerSolution {

  override def expect = 137846528820L
  
  def solve = {
    def nextRow(prevRow: List[Long]): List[Long] = {
      1L :: (0 to prevRow.length - 2).map(n => prevRow(n) + prevRow(n + 1)).toList ::: List(1L)
    }
    
    var row = List(1L)
    while (row.length < 41) {
      row = nextRow(row)
    }
    row(20)
  }

}
