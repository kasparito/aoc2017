import scala.io.Source

object Day01 extends App {

  val numbers = Source.fromFile("input/day01.txt").map(_.toString.toInt).toIndexedSeq

  def calculate(distance: Int) =
    numbers
      .zipWithIndex
      .collect { case (n, index) if n == numbers((index + distance) % numbers.length) => n }
      .sum

  println(calculate(1))
  println(calculate(numbers.length / 2))
}
