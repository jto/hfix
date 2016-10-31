package test

trait ListF[+A, +S]
trait Nil extends ListF[Nothing, Nothing]
object Nil extends Nil
case class Cons[A, +S](x: A, xs: S) extends ListF[A, S]

object list {
  type List[A] = Fix[ListF[A, ?]]
  def nil[A] = Fix[ListF[A, ?]](Nil)
  def cons[A] = (x:A, xs: List[A]) => Fix[ListF[A, ?]](Cons(x, xs))
}

object hlist {
  def hnil = HFix[ListF[Nil, ?], INil](Nil)
  def hcons[X, XS <: Inductive](x: X, xs: XS) = HFix[ListF[X, ?], XS](Cons(x, xs))

  val test = hcons(1, hcons("bar", hnil))
}