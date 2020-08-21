package Ch4

import cats.data.Writer

object WriterInterface {


  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)
  def factorial(n: Int): Int = {
    val ans = slowly(if(n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  // Monad of the form M[_}
  type Logged[A] = Writer[Vector[String],A]

  import cats.syntax.applicative._
  import cats.instances.vector._
  import cats.syntax.writer._

  def factorialWriter(n: Int): Logged[Int] = {
    for {
      ans <- if(n == 0) { 1.pure[Logged]} else {
        slowly(factorialWriter(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }
}

object WriterEx {

  import WriterInterface._
  import cats.syntax.writer._
  def main (args: Array[String]): Unit = {
    //factorial(7)
    println(factorialWriter(7))

    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    println(Await.result(Future.sequence(Vector(
      Future(factorialWriter(28).run),
      Future(factorialWriter(30))
    )), 10.seconds))
  }

}
