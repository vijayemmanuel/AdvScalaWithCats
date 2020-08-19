package Ch2

object Typeclass {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

}

object SetTypeClassInstances {

  import Typeclass._
  implicit def unionSet[A] = new Monoid[Set[A]] {
    def combine (x: Set[A], y: Set[A]) = x union( y)
    def empty: Set[A] = Set.empty
  }

  implicit def unionInt = new Monoid[Int] {
    def combine (x: Int, y: Int) = x +( y)
    def empty: Int = 0
  }
}

object MonoidInterface {
  import Typeclass._
  object Monoid {
    def apply[A](implicit monoid: Monoid[A]) =
      monoid
  }
}

object SetEx {

  import MonoidInterface._
  import SetTypeClassInstances.unionSet
  def main ( args: Array[String]): Unit = {
    println(Monoid[Set[Int]].combine(Set(1,2,3),Set(2,4)))
  }
}
