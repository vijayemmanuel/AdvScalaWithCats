package Ch4

import cats.Monad

object TreeMonadEx {

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  val treeMonad =
    new Monad[Tree] {
      def flatMap[A, B](opt: Tree[A])(fn: A => Tree[B]): Tree[B] =
        opt match {
          case Branch(l: A,r: A) =>  Branch(flatMap(l)(fn), flatMap(r)(fn))
          case Leaf(value: A) => fn(value)
        }
      def pure[A](opt: A): Tree[A] =
        opt match {
          case Branch(l: A,r: A) => _
          case Leaf(value: A) => _
          case value : A => Leaf(value)
        }

      def tailRecM[A, B](a: A)(fn: A => Tree[Either[A, B]]): Tree[B] =
        fn(a) match {
          case Branch(l, r) => Branch(
            flatMap(l) {
              case Left(l)  => tailRecM(l)(fn)
              case Right(l) => pure(l)
            },
            flatMap(r) {
              case Left(r)  => tailRecM(r)(fn)
              case Right(r) => pure(r)
            }
          )
          case Leaf(Left(value)) =>
            tailRecM(value)(fn)
          case Leaf(Right(value)) =>
            Leaf(value)
        }
    }

}
