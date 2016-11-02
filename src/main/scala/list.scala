package test

import shapeless.{ DepFn1, Poly, Poly1 }
import shapeless.PolyDefns._
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
  val hnil = HFix[ListF[Nil, ?], INil](Nil)
  def hcons[X, XS <: Inductive](x: X, xs: XS) = HFix[ListF[X, ?], XS](Cons(x, xs))

  trait Cata[HF, L <: Inductive] extends DepFn1[L]

  trait LowPriorityCata {
    implicit def hfixCata[HF <: Poly, F[_], T <: Inductive, OutC](implicit fc: Functor[F], cata: Cata.Aux[HF, T, OutC], f: Case1[HF, F[OutC]]) =
      new Cata[HF, HFix[F, T]] {
        type Out = f.Result
        def apply(t: HFix[F, T]) =
          f(fc.map(t.f)(t => cata(t)))
      }
  }

  object Cata extends LowPriorityCata{
    type Aux[HF <: Poly, L <: Inductive, Out0] = Cata[HF, L] { type Out = Out0 }

    def apply[HF <: Poly, L <: Inductive](implicit c: Cata[HF, L]): Aux[HF, L, c.Out] = c

    implicit def lastCata[HF <: Poly, F[_]](implicit fc: Functor[F], f: Case1[HF, F[INil]]): Aux[HF, HFix[F, INil], f.Result] =
      new Cata[HF, HFix[F, INil]] {
        type Out = f.Result
        def apply(t: HFix[F, INil]) = f(t.f)
      }
  }

  def cata[HF <: Poly, L <: Inductive](l: L, f: HF)(implicit c: Cata[HF, L]) =
    c(l)
}

object listtest {
  import list._
  val xs = cons(1, cons(1, nil))

  val sumList =
    cata[Int, ListF[Int, ?]] {
      case Nil => 0
      case Cons(x, n) => x + n
    } _
}

object hlisttest {
  import hlist._

  val hs = hcons(1, hcons("bar", hnil))
  val xs = hcons(1, hcons(1, hnil))

  object plus extends Poly1 {
    implicit def caseNil =
      at[ListF[Nil,INil]] { _ => 0 }
    implicit def caseInt =
      at[ListF[Int, Int]] {
        case Cons(x, n) => x + n
      }
  }

  val sum0 = cata(hnil, plus)
  val sum1 = cata(hcons(1, hnil), plus)
  val sum2 = cata(xs, plus)
}