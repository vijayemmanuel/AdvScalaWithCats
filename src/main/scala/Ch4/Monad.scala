package Ch4

import scala.language.higherKinds

trait Monad[F[_]] { // F[_] is an endofunctor
  def pure[A](value :A):F[A]  // like monoid

  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

  // Express map in terms of flatmap and pure
  def map[A,B](fa: F[A])(f: A => B) : F[B] = {
    flatMap(fa)(a => pure(f(a)))
  }
}