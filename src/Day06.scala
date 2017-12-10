import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source


object Day06 extends App {
  val input = Source.fromFile("input/day06.txt").mkString.split("\\s+").map(_.toInt).toList

  val seen = mutable.Set[List[Int]]()

  @tailrec
  def redistribute(buffers: List[Int]): List[Int] =
    if (!seen.add(buffers)) {
      buffers
    } else {
      val max = buffers.max
      val index = buffers.indexOf(max)
      val newBuffers = mutable.IndexedSeq(buffers:_*)
      newBuffers(index) = 0
      1 to max foreach { n => newBuffers((index + n) % buffers.size) += 1 }
      redistribute(newBuffers.toList)
    }

  val loopBuffer = redistribute(input)
  println(seen.size)

  seen.clear()
  redistribute(loopBuffer)
  println(seen.size)
}
