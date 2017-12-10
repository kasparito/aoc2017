import scala.collection.mutable
import scala.io.Source


object Day05 extends App {
  val input = mutable.IndexedSeq(Source.fromFile("input/day05.txt").getLines().map(_.toInt).toSeq:_*)

  var moves = 0
  var position = 0

  while (0 <= position && position < input.size) {
    val move = input(position)
    if (move >= 3)
      input(position) = move - 1
    else
      input(position) = move + 1
    position += move
    moves += 1
  }

  println(moves)
}
