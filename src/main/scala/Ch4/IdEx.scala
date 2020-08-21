package Ch4

import scala.language.higherKinds
import cats.{Id, Monad}

object IdInstance {

  type Id[A] = A

  def pure[A](value : A) : A = value

  def flatMap[A, B](a: A)(f: A => Id[B]): Id[B] = f(a)

  def map [A, B](a: A)(f: A => B): Id[B] = f(a)

}

object IdEx {
  def main(args: Array[String]): Unit = {
    import IdInstance.Id
    import IdInstance._

    println(pure(3))
    println(pure(3).isInstanceOf[Id[Int]])
  }
}

