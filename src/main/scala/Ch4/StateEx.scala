package Ch4

import cats.data.State

object Calc {

  type CalcState[A] = State[List[Int], A]
  def evalOne(sym: String): CalcState[Int] = {
    sym match {
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case num => operand(num.toInt)
    }
  }

  def evalAll(input: List[String]): CalcState[Int] = {
    import cats.syntax.applicative._
    //input.map(x => evalOne(x)).last // Why is it wrong ??

    input.foldLeft(0.pure[CalcState]) { (a, b) =>
      a flatMap (_ => evalOne(b))
    }
  }

  def operand(value: Int): CalcState[Int] = {
    State(state => (value :: state , value))
  }
  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State {
      case a :: b :: tail => {
        val ans = func(a, b)
        (ans :: tail, ans)
      }
      case _ => sys.error("Fail!")
    }
}

object StateEx {
  import Calc._
  def main ( args: Array[String]): Unit = {
    println(evalOne("42").runA(Nil).value)


    val program = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      ans <- evalOne("+")
    } yield ans
    println(program.runA(Nil).value)


    val program2 = evalAll(List("1", "2", "+", "3", "*"))
    println(program2.runA(Nil).value)

  }



}

