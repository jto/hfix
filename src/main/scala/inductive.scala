package test

import cats.Functor

package fix {
  case class Fix[F[_]](f: F[Fix[F]])

  object syntax {
    def cata[A, F[_]](f: F[A] => A)(t: Fix[F])(implicit fc: Functor[F]): A =
      f(fc.map(t.f)(cata[A, F](f)))
  }
}

package hfix {

  import shapeless.{ DepFn1, Poly, Poly1 }
  import shapeless.PolyDefns._

  trait Inductive
  case class HFix[F[_], R <: Inductive](f: F[R]) extends Inductive

  package ops {
    trait Cata[HF, L <: Inductive] extends DepFn1[L]

    trait LowPriorityCata {
      implicit def hfixCata[HF <: Poly, F[_], T <: Inductive, OutC](implicit fc: Functor[F], cata: Cata.Aux[HF, T, OutC], f: Case1[HF, F[OutC]]) =
        new Cata[HF, HFix[F, T]] {
          type Out = f.Result
          def apply(t: HFix[F, T]) =
            f(fc.map(t.f)(t => cata(t)))
        }
    }

    object Cata extends LowPriorityCata {
      type Aux[HF <: Poly, L <: Inductive, Out0] = Cata[HF, L] { type Out = Out0 }

      def apply[HF <: Poly, L <: Inductive](implicit c: Cata[HF, L]): Aux[HF, L, c.Out] = c

      implicit def nothingCata[HF <: Poly, R]: Aux[HF, Uninhabited, R] =
        new Cata[HF, Uninhabited] {
          type Out = R
          def apply(t: Uninhabited) = t
        }
    }
  }

  object syntax {
    import ops._
    def cata[HF <: Poly, L <: Inductive](l: L, f: HF)(implicit c: Cata[HF, L]) =
      c(l)
  }
}

package object hfix {
  // see Miles Sabin's workaround at https://issues.scala-lang.org/browse/SI-9453
  type Uninhabited = Nothing { type T = Unit }
}
