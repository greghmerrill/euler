abstract class EulerSolution extends App {
  
  def solve: Any
  def expect: Any = "Unkown" // subclass should override with actual answer from Project Euler

  util.Properties.setProp("scala.time", "true")
  Console.println("Solving ...")
  val solution = solve
  Console.println("Answer: " + solution)

  if (expect != "Unkown" && expect != solution) {
    Console.println("Answer " + expect + " from Project Euler not matched!");
  }

}
