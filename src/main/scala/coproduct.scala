package test

import hfix._

object coproduct {
  sealed trait Cocons[+H, +T]
  final case class Inl[+H, +T](head: H) extends Cocons[H, T]
  final case class Inr[+H, +T](tail: T) extends Cocons[H, T]

  type CNil = HFix[Nothing, Nothing]
  type :+:[H, T <: Inductive] = HFix[Cocons[H, ?], T]

  trait Inject[C <: Inductive, I] {
    def apply(i: I): C
  }

  object Inject {
    def apply[C <: Inductive, I](implicit inject: Inject[C, I]): Inject[C, I] = inject

    implicit def tlInject[H, T <: Inductive, I](implicit tlInj: Inject[T, I]): Inject[H :+: T, I] = new Inject[H :+: T, I] {
      def apply(i: I): H :+: T = HFix(Inr(tlInj(i)))
    }

    implicit def hdInject[H, T <: Inductive]: Inject[H :+: T, H] = new Inject[H :+: T, H] {
      def apply(i: H): H :+: T = HFix(Inl(i))
    }
  }

  class MkCoproduct[C <: Inductive] {
    def apply[T](t: T)(implicit inj: Inject[C, T]): C = inj(t)
  }

  def Coproduct[C <: Inductive] = new MkCoproduct[C]
}
