package test

case class Fix[F[_]](f: F[Fix[F]])

trait Inductive
case class HFix[F[_], R <: Inductive](f: F[R]) extends Inductive
trait INil extends Inductive