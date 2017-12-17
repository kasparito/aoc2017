import scala.io.Source


object Day09 extends App {
  val input = Source.fromFile("input/day09.txt")

  def consume(chars: Iterator[Char], level: Int): (Int, Int) =
    if (!chars.hasNext)
      (0, 0)
    else
      chars.next() match {
        case '{' => consume(chars, level + 1)
        case '}' =>
          val (sum, garbage) = consume(chars, level - 1)
          (level + sum, garbage)
        case '<' =>
          val newGarbage = consumeGarbage(chars, level)
          val (sum, garbage) = consume(chars, level)
          (sum, garbage + newGarbage)
        case _ => consume(chars, level)
      }

  def consumeGarbage(chars: Iterator[Char], level: Int): Int =
    if (!chars.hasNext)
      0
    else
      chars.next() match {
        case '>' => 0
        case '!' => chars.next(); consumeGarbage(chars, level)
        case _ => 1 + consumeGarbage(chars, level)
      }

  println(consume(input, 0))
}
