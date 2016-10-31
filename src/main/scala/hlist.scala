package test

object hlist {
  def hnil = HFix[ListF[Nil, ?], INil](Nil)
  def hcons[X, XS <: Inductive](x: X, xs: XS) = HFix[ListF[X, ?], XS](Cons(x, xs))

  val test = hcons(1, hcons("bar", hnil))
}