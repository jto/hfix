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