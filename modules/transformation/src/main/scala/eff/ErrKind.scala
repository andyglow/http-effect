package eff

import scala.language.implicitConversions


sealed trait ErrKind extends Any
object ErrKind {
  final case class string(value: String) extends AnyVal with ErrKind
  final case class throwable(value: Throwable) extends AnyVal with ErrKind
  final case class err(value: Err) extends AnyVal with ErrKind
  implicit def apply(x: String): ErrKind = string(x)
  implicit def apply(x: Throwable): ErrKind = throwable(x)
  implicit def apply(x: Err): ErrKind = err(x)
}
