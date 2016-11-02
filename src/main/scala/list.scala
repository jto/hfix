package test

import shapeless.{ DepFn1, Poly }
import cats.Functor

trait BaseList
trait ListF[+A, +S] extends BaseList
trait Nil extends ListF[Nothing, Nothing]
object Nil extends Nil
case class Cons[A, +S](x: A, xs: S) extends ListF[A, S]

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
  type List[A] = Fix[ListF[A, ?]]
  def nil[A] = Fix[ListF[A, ?]](Nil)
  def cons[A] = (x: A, xs: List[A]) => Fix[ListF[A, ?]](Cons(x, xs))

  def cata[A, F[_]](f: F[A] => A)(t: Fix[F])(implicit fc: Functor[F]): A =
    f(fc.map(t.f)(cata[A, F](f)))

}

object hlist {
  def hnil = HFix[ListF[Nil, ?], INil](Nil)
  def hcons[X, XS <: Inductive](x: X, xs: XS) = HFix[ListF[X, ?], XS](Cons(x, xs))
}

object test {
  import list._, hlist._
  val xs = cons(1, cons(1, nil))

  val sumList =
    cata[Int, ListF[Int, ?]] {
      case Nil => 0
      case Cons(x, n) => x + n
    } _

  val hs = hcons(1, hcons("bar", hnil))
}