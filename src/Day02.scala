import scala.io.Source

object Day02 extends App {
  val spreadSheet = Source
    .fromFile("input/day02.txt")
    .getLines()
    .map(_.split("\\s").map(_.toInt).toIndexedSeq)
    .toIndexedSeq

  val part1 = spreadSheet.map(row => row.max - row.min).sum
  println(part1)

  val part2 = {
    for {
      row <- spreadSheet
      a <- row
      b <- row
      if a != b
      if (a % b) == 0
    } yield a / b
  }.sum
  println(part2)
}
