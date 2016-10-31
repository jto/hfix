package test

object coproduct {
  sealed trait :+:[+H, +T]
  final case class Inl[+H, +T](head : H) extends :+:[H, T]
  final case class Inr[+H, +T](tail : T) extends :+:[H, T]

  def r[T](v: T) = HFix[:+:[T, ?], INil](Inl(v))
  def l[T] = new {
    def apply[C <: Inductive](c: C) = HFix[:+:[T, ?], C](Inr(c))
  }
  val test = l[Double](l[Int](r("bar")))
}