package test

case class Fix[F[_]](f: F[Fix[F]])

trait Inductive
case class HFix[F[_], R <: Inductive](f: F[R]) extends Inductive
trait INil extends Inductive

// def cata[A, F[_]](f: F[A] => A)(t: Mu[F])(implicit fc: Functor[F]): A = {
//   f(fc.fmap(t.out, cata[A, F](f)))
// }