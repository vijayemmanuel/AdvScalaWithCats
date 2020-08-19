package Ch2

import cats.Monoid
import cats.instances.int._
import cats.instances.option._
import cats.syntax.semigroup._

case class Order(totalCost: Double, quantity: Double)

object OrderInstance {
  implicit val order = new Monoid[Order] {
    def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity+ y.quantity)
    def empty: Order = Order(0,0)
  }
}
object ListInterface {
  def add[A: Monoid](xs: List[A]) : A = {
    xs.foldLeft(implicitly[Monoid[A]].empty)(_ |+| _)
  }
}

object SuperAdderEx {

  def main ( args: Array[String]): Unit = {

    import ListInterface._
    import OrderInstance._
    println(add(List(1,2,3)))
    println(add(List(Some(1),Some(2),None, Some(3))))
    println(add(List(Order(1,2),Order(2,2),Order(4,2))))

    println(Order(1,2) |+| Order(2,2) |+| Order(4,2))
    //def  add(items: List[Int]): Int = {
    //  items.foldLeft(0)( _ + _)
    //}
  }

}
