package eff

import scala.util.{Either, Try}
import eff.internal.{ VersionFix => vfix }

trait Functor[E[_]] {

  def map[A, B](e: E[A])(f: A => B): E[B]

  def flatMap[A, B](e: E[A])(f: A => E[B]): E[B]
}

trait LowPriorityFunctors {

  implicit object OptionF extends Functor[Option] {
    override def map[A, B](e: Option[A])(f: A => B): Option[B] = e map f
    override def flatMap[A, B](e: Option[A])(f: A => Option[B]): Option[B] = e flatMap f
  }

  implicit object TryF extends Functor[Try] {
    override def map[A, B](e: Try[A])(f: A => B): Try[B] = e map f
    override def flatMap[A, B](e: Try[A])(f: A => Try[B]): Try[B] = e flatMap f
  }

  implicit object ResultF extends Functor[Result] {
    override def map[A, B](e: Result[A])(f: A => B): Result[B] = e map f
    override def flatMap[A, B](e: Result[A])(f: A => Result[B]): Result[B] = e flatMap f
  }

  implicit def eitherF[L]: Functor[({ type E[T] = Either[L, T] })#E] = new Functor[({ type E[T] = Either[L, T] })#E] {
    override def map[A, B](e: Either[L, A])(f: A => B): Either[L, B] = vfix.map(e, f)
    override def flatMap[A, B](e: Either[L, A])(f: A => Either[L, B]): Either[L, B] = vfix.flatMap(e, f)
  }


}

object Functor extends LowPriorityFunctors {

  def apply[E[_]: Functor]: Functor[E] = implicitly
}