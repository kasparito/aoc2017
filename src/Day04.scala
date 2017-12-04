import scala.io.Source


object Day04 extends App {

  val passPhrases = Source.fromFile("input/day04.txt").getLines().map(_.split("\\s").toList).toList

  println(passPhrases.count(p => p.size == p.toSet.size))

  println(passPhrases.map(_.map(w => new String(w.toCharArray.sorted))).count(p => p.size == p.toSet.size))
}
