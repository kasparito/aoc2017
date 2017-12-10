import scala.collection.mutable
import scala.io.Source


object Day08 extends App {

  val registers = mutable.Map[String, Int]()

  sealed trait Operator {
    def eval(o1: Operand, o2: Operand): Int
  }
  object Add extends Operator {
    def eval(o1: Operand, o2: Operand) = o1.eval + o2.eval
  }
  object Subtract extends Operator {
    def eval(o1: Operand, o2: Operand) = o1.eval - o2.eval
  }

  sealed trait Operand {
    def eval: Int
  }
  case class Constant(value: Int) extends Operand {
    def eval = value
  }
  case class Register(reg: String) extends Operand {
    def eval = registers.getOrElse(reg, 0)
  }
  object Operand {
    def apply(s: String): Operand =
      if (s.matches("-?\\d+"))
        Constant(s.toInt)
      else
        Register(s)
  }

  case class Predicate(c1: Operand, op: (Int, Int) => Boolean, c2: Operand) {
    def eval = op.apply(c1.eval, c2.eval)
  }

  case class Operation(reg: Register, op: Operator, operand: Operand, predicate: Predicate)

  val instruction = """(\w+) (inc|dec) (\S+) if (\S+) ([<>=!][=]?) (\S+)""".r

  val max = Source
    .fromFile("input/day08.txt")
    .getLines()
    .map {
      case instruction(reg, op, operand, c1, cop, c2) =>
        Operation(
          Register(reg),
          op match {
            case "inc" => Add
            case "dec" => Subtract
          },
          Operand(operand),
          Predicate(
            Operand(c1),
            cop match {
              case "<" => _ < _
              case ">" => _ > _
              case "<=" => _ <= _
              case ">=" => _ >= _
              case "==" => _ == _
              case "!=" => _ != _
            },
            Operand(c2)
          )
        )
    }
    .flatMap {
      case Operation(reg, op, operand, pred) if pred.eval =>
        registers(reg.reg) = op.eval(reg, operand)
        Some(registers.values.max)
      case _ =>
        None
    }
    .max

  println(registers.values.max)
  println(max)
}
