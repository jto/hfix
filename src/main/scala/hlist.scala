package test

import hfix._
object hlist {
  type HNil = HFix[ListF[Nil, ?], INil]
  type ::[X, XS <: Inductive] = HFix[ListF[X, ?], XS]

  val hnil: HNil = HFix[ListF[Nil, ?], INil](Nil)
  def hcons[X, XS <: Inductive](x: X, xs: XS): X :: XS = HFix[ListF[X, ?], XS](Cons(x, xs))
}