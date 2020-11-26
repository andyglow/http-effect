package eff

import scala.reflect.ClassTag


final class EvalEither[A](
  private val dummy: Boolean = true) extends AnyVal {

  def apply[B](f: => B)(implicit ct: ClassTag[A]): Either[A, B] =
    try Right(f) catch {
      case ct(t) => Left(t)
    }
}