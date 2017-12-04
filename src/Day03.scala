import scala.collection.mutable

object Day03 extends App {
  val input = 347991

  def distance(n: Int): Int =
    if (n == 1)
      0
    else
      distance(n, 1)

  def distance(n: Int, ring: Int): Int = {
    val max = (ring * 2 + 1) * (ring * 2 + 1) + 1
    if (n < max)
      math.abs((n + ring * 8 - max) % (ring * 2) - ring + 1) + ring
    else
      distance(n, ring + 1)
  }

  println(distance(input))

  val board = mutable.Map[(Int, Int), Int]((0, 0) -> 1)
  var dir = (0, 1)
  def find(n: Int, x: Int, y: Int, nextRing: Boolean): Int = {
    val v = board.getOrElseUpdate((x, y), {
      board.getOrElse((x + 1, y + 1), 0) +
        board.getOrElse((x + 1, y), 0) +
        board.getOrElse((x + 1, y - 1), 0) +
        board.getOrElse((x, y + 1), 0) +
        board.getOrElse((x, y - 1), 0) +
        board.getOrElse((x - 1, y + 1), 0) +
        board.getOrElse((x - 1, y), 0) +
        board.getOrElse((x - 1, y - 1), 0)
    })
    if (v > n) {
      v
    } else {
      dir =
        if (nextRing)
          (0, 1)
        else if (x < 0 && x == -y)
          (0, -1)
        else if (x < 0 && x == y)
          (1, 0)
        else if (x > 0 && x == y)
          (-1, 0)
        else if (x > 0 && -x == y)
          (1, 0)
        else
          dir
      find(n, x + dir._1, y + dir._2, x > 0 && -x == y)
    }
  }

  println(find(input, 1, 0, nextRing = false))
}
