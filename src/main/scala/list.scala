package test

trait ListF[+A, +S]
case class Nil[+S]() extends ListF[Nothing, S]
case class Cons[A, +S](x: A, xs: S) extends ListF[A, S]

import cats.Functor

object ListF {
  implicit def nilFunctor =
    new Functor[Nil] {
      def map[A, B](fa: Nil[A])(f: A => B): Nil[B] = Nil()
    }

  implicit def consFunctor[T] =
    new Functor[Cons[T, ?]] {
      def map[A, B](fa: Cons[T, A])(f: A => B): Cons[T, B] = Cons(fa.x, f(fa.xs))
    }

  implicit def listFFunctor[T] =
    new Functor[ListF[T, ?]] {
      def map[A, B](fa: ListF[T, A])(f: A => B): ListF[T, B] =
        fa match {
          case Nil() => Nil()
          case Cons(x, xs) => Cons(x, f(xs))
        }
    }
}

object list {
  import fix._
  type List[A] = Fix[ListF[A, ?]]
  def nil[A] = Fix[ListF[A, ?]](Nil())
  def cons[A] = (x: A, xs: List[A]) => Fix[ListF[A, ?]](Cons(x, xs))
}
