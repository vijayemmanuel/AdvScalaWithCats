package Ch3

import cats.Functor

object Typeclass {
  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A])  extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]
}

object TreeInstances {

  import Typeclass._

  implicit val IntTreeInstance = new Functor[Tree] { self =>
    def map[A,B](fa: Tree[A])(f : A => B): Tree[B] = fa match {
      case Branch(l: Tree[A], r: Tree[A]) => Branch(self.map(l)(f), self.map(r)(f))
      case Leaf(value: A) => Leaf(f(value))
    }
  }


}

object TreeInterfaces {
  import Typeclass._
  def applyFunctiontoTree[A,B](t: Tree[A], f: A => B)(implicit v: Functor[Tree]) = {
    v.map(t)(f)
  }

}

object TreeEx {

  def main(args: Array[String]): Unit = {

    import Typeclass._
    import TreeInstances._

    val tree :Tree[Int] = Branch(Leaf(1),Branch(Leaf(1), Leaf(1)))

    println(TreeInterfaces.applyFunctiontoTree(tree,  (x: Int) => x * 2))

    import cats.syntax.functor._
    println(tree.map(_ * 2))

  }
}