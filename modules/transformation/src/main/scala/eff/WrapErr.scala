package eff

import scala.util._


trait WrapErr[E[_]] {

  def apply[T](x: ErrKind): E[T]
}

object WrapErr {

  def apply[E[_]: WrapErr]: WrapErr[E] = implicitly

  def apply[E[_]: WrapErr, R](x: ErrKind): E[R] = WrapErr[E].apply[R](x)

  implicit val resultWE: WrapErr[Result] = new WrapErr[Result] {
    override def apply[T](x: ErrKind): Result[T] = x match {
      case ErrKind.string(x)                  => Err.Msg(x)
      case ErrKind.throwable(ThrowableErr(x)) => x
      case ErrKind.throwable(x)               => Err.Exception(x)
      case ErrKind.err(x)                     => x
    }
  }

  implicit val opaqueWE: WrapErr[Opaque] = new WrapErr[Opaque] {
    override def apply[T](x: ErrKind): Opaque[T] = x match {
      case ErrKind.string(x)    => throw ThrowableErr(x)
      case ErrKind.throwable(x) => throw x
      case ErrKind.err(x)       => throw ThrowableErr(x.message)
    }
  }

  implicit val optionWE: WrapErr[Option] = new WrapErr[Option] {
    override def apply[T](x: ErrKind): Option[T] = None
  }

  implicit val tryWE: WrapErr[Try] = new WrapErr[Try] {
    override def apply[T](x: ErrKind): Try[T] = x match {
      case ErrKind.string(x)                  => Failure[T](ThrowableErr(x))
      case ErrKind.throwable(x)               => Failure[T](x)
      case ErrKind.err(Err.Exception(x, Nil)) => Failure[T](x)
      case ErrKind.err(Err.Msg(x, Nil))       => Failure[T](ThrowableErr(x))
      case ErrKind.err(x)                     => Failure[T](ThrowableErr(x))
    }
  }

  implicit val leftStringEV: WrapErr[({ type E[T] = Either[String, T] })#E] = new WrapErr[({ type E[T] = Either[String, T] })#E] {
    override def apply[T](x: ErrKind): Either[String, T] = x match {
      case ErrKind.string(x)                                      => Left(x)
      case ErrKind.throwable(ThrowableErr(Err.Msg(x, Nil)))       => Left(x)
      case ErrKind.throwable(ThrowableErr(Err.Exception(x, Nil))) => Left(x.toString)
      case ErrKind.throwable(ThrowableErr(x))                     => Left(x.toString)
      case ErrKind.throwable(x)                                   => Left(x.toString)
      case ErrKind.err(Err.Exception(x, Nil))                     => Left(x.toString)
      case ErrKind.err(Err.Msg(x, Nil))                           => Left(x)
      case ErrKind.err(x)                                         => Left(x.message)
    }
  }

  implicit val leftThrowableEV: WrapErr[({ type E[T] = Either[Throwable, T] })#E] = new WrapErr[({ type E[T] = Either[Throwable, T] })#E] {
    override def apply[T](x: ErrKind): Either[Throwable, T] = x match {
      case ErrKind.string(x)                                      => Left(ThrowableErr(x))
      case ErrKind.throwable(ThrowableErr(Err.Exception(x, Nil))) => Left(x)
      case ErrKind.throwable(x)                                   => Left(x)
      case ErrKind.err(Err.Exception(x, Nil))                     => Left(x)
      case ErrKind.err(Err.Msg(x, Nil))                           => Left(ThrowableErr(x))
      case ErrKind.err(x)                                         => Left(ThrowableErr(x.message))
    }
  }

  implicit val leftErrEV: WrapErr[({ type E[T] = Either[Err, T] })#E] = new WrapErr[({ type E[T] = Either[Err, T] })#E] {
    override def apply[T](x: ErrKind): Either[Err, T] = x match {
      case ErrKind.string(x)                  => Left(Err.Msg(x))
      case ErrKind.throwable(ThrowableErr(x)) => Left(x)
      case ErrKind.throwable(x)               => Left(Err.Exception(x))
      case ErrKind.err(x)                     => Left(x)
    }
  }
}
