import scala.io.Source


case class Program(name: String, weight: Int, children: Set[String])


object Day07 extends App {
  val program = """(\w+) \((\d+)\)( -> .*)?""".r

  val input = Source
    .fromFile("input/day07.txt")
    .getLines()
    .map {
      case program(name, weight, children) =>
        name -> Program(
          name,
          weight.toInt,
          Option(children).view.flatMap(_.substring(4).split(",\\s*")).toSet
        )
    }
    .toMap

  val children = input.flatMap(_._2.children).toSet

  val root = (input.keySet -- children).head

  println(root)

  def weight(name: String): Int = {
    val program = input(name)
    val weights = program.children.toList.map(weight)
    if (weights.toSet.size > 1)
      println(s"$program imbalance: $weights, children: ${program.children.flatMap(input.get)}")
    program.weight + weights.sum
  }

  weight(root)
}
