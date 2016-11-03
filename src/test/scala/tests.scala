package test

import org.scalatest._
import shapeless.Poly1
import hfix._
import shapeless.test._

class RulesSpec extends WordSpec with Matchers {

  "Fix" should {

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

    "implement hlist and sum" in {
      import hlist._, syntax._

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

      val sum0 = cata(hnil, plus)
      val sum1 = cata(hcons(1, hnil), plus)
      val sum2 = cata(xs, plus)

      sum0 shouldBe 0
      sum1 shouldBe 1
      sum2 shouldBe 6
    }

    "implement a coproduct" in {
      import coproduct._
      Coproduct[Int :+: String :+: INil](1) shouldBe HFix(Inl(1))
      Coproduct[Int :+: String :+: INil]("bar") shouldBe HFix(Inr(HFix(Inl("bar"))))
      illTyped("Coproduct[Int :+: String :+: INil](1.2)")
    }
  }
}