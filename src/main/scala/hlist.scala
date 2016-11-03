package test

import hfix._
object hlist {
  val hnil = HFix[ListF[Nil, ?], INil](Nil)
  def hcons[X, XS <: Inductive](x: X, xs: XS) = HFix[ListF[X, ?], XS](Cons(x, xs))
}