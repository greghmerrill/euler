abstract class EulerSolution extends App {
  def solve: Any

  util.Properties.setProp("scala.time", "true")
  Console.println("Answer: " + solve)
}
