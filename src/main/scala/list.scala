package test

trait BaseList
trait ListF[+A, +S] extends BaseList
trait Nil extends ListF[Nothing, Nothing]
object Nil extends Nil
case class Cons[A, +S](x: A, xs: S) extends ListF[A, S]

import cats.Functor

object ListF {
  implicit def listFFunctor[T] =
    new Functor[ListF[T, ?]] {
      def map[A, B](fa: ListF[T, A])(f: A => B): ListF[T, B] =
        fa match {
          case Nil => Nil
          case Cons(x, xs) => Cons(x, f(xs))
        }
    }
}

object list {
  import fix._
  type List[A] = Fix[ListF[A, ?]]
  def nil[A] = Fix[ListF[A, ?]](Nil)
  def cons[A] = (x: A, xs: List[A]) => Fix[ListF[A, ?]](Cons(x, xs))
}