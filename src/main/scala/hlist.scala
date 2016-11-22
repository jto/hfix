package test

import hfix._
object hlist {
  type HNil = HFix[Nil, Uninhabited]
  type ::[X, XS <: Inductive] = HFix[Cons[X, ?], XS]

  val hnil: HNil = HFix[Nil, Uninhabited](Nil())
  def hcons[X, XS <: Inductive](x: X, xs: XS): X :: XS = HFix[Cons[X, ?], XS](Cons(x, xs))
}
