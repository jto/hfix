package test

import org.scalatest._

class RulesSpec extends WordSpec with Matchers {

  import shapeless.Poly1
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

  "Fix" should {
    object list {
      import fix._
      type List[A] = Fix[ListF[A, ?]]
      def nil[A] = Fix[ListF[A, ?]](Nil)
      def cons[A] = (x: A, xs: List[A]) => Fix[ListF[A, ?]](Cons(x, xs))
    }

    "implement list and sum" in {
      import list._
      import fix._, syntax._

      val xs = cons(1, cons(2, cons(3, nil)))

      val sumList =
        cata[Int, ListF[Int, ?]] {
          case Nil => 0
          case Cons(x, n) => x + n
        } _

      sumList(xs) shouldBe 6
    }
  }

  "HFix" should {
    object hlist {
      import hfix._
      val hnil = HFix[ListF[Nil, ?], INil](Nil)
      def hcons[X, XS <: Inductive](x: X, xs: XS) = HFix[ListF[X, ?], XS](Cons(x, xs))
    }

    "implement hlist and sum" in {
      import hlist._
      import hfix._, syntax._

      val hs = hcons(1, hcons("bar", hnil))
      val xs = hcons(1, hcons(2, hcons(3, hnil)))

      object plus extends Poly1 {
        implicit def caseNil =
          at[ListF[Nil, INil]] { _ => 0 }
        implicit def caseInt =
          at[ListF[Int, Int]] {
            case Cons(x, n) => x + n
          }
      }

      object plus1 extends Poly1 {
        implicit def caseInt = at[Int] { _ + 1 }
      }

      val sum0 = cata(hnil, plus)
      val sum1 = cata(hcons(1, hnil), plus)
      val sum2 = cata(xs, plus)

      sum0 shouldBe 0
      sum1 shouldBe 1
      sum2 shouldBe 6
    }
  }
}