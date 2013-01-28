abstract class EulerSolution extends App {
  util.Properties.setProp("scala.time", "true")
  def solve : Any
  Console.println("Answer: " + solve)
}