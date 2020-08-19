package Ch2

object Typeclass {

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

}

object BooleanTypeClassInstances {

  import Typeclass._
  implicit val BooleanAndBoolean = new Monoid[Boolean] {
    def combine (x: Boolean, y: Boolean) = x && y
    def empty: Boolean = true
  }

  implicit val BooleanOrBoolean = new Monoid[Boolean] {
    def combine (x: Boolean, y: Boolean) = x || y
    def empty: Boolean = true
  }
}

object MonoidInterface {
  import Typeclass._
  object Monoid {
    def apply[A](implicit monoid: Monoid[A]) =
      monoid
  }
}

object BooleanEx {

  import MonoidInterface._
  //import BooleanTypeClassInstances.BooleanOrBoolean
  import BooleanTypeClassInstances.BooleanAndBoolean
  def main ( args: Array[String]): Unit = {
    println(Monoid[Boolean].combine(true,false))
  }
}
