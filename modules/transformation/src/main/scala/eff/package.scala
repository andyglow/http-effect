
import scala.util._

package object eff extends ScalaVersionSpecific {

  type Opaque[T] = T

  implicit class EffResultOps[T](private val x: Result[T]) extends AnyVal {

    def getOrThrow: T = x match {
      case Ok(x)                 => x
      case Err.Exception(x, Nil) => throw x
      case err: Err              => throw ThrowableErr(err.message)
    }

    def to[E[_]: Eff: WrapErr]: E[T] = x match {
      case Ok(r)  => Eff.ok(r)
      case e: Err => Eff.ko(e)
    }
  }

  implicit class EffEitherOps[A, B](private val x: Either[A, B]) extends AnyVal {

    def filterOrElse[AA >: A](p: B => Boolean, zero: => AA): Either[AA, B] = x match {
      case Right(b) => if (p(b)) x else Left(zero)
      case Left(_)  => x
    }

    def otherwise[AA, BB >: B](alt: A => Either[AA, BB]): Either[AA, BB] = x.fold(alt, Right(_))

    def :>[AA, BB >: B](alt: => Either[AA, BB]): Either[AA, BB] = x.fold(_ => alt, Right(_))

    def getOrThrow(implicit TH: Throw[A]): B = x.fold[B](TH.apply, identity)
  }


  implicit class EffLeftStringOps[B](val x: Either[String, B]) extends AnyVal {

    def asThrowable: Either[Throwable, B] = x.left map ThrowableErr.apply

    def to[E[_]: Eff: WrapErr]: E[B] = x match {
      case Right(r) => Eff.ok(r)
      case Left(e)  => Eff.ko(e)
    }
  }

  implicit class EffLeftThrowableOps[B](val x: Either[Throwable, B]) extends AnyVal {

    def asString: Either[String, B] = x.left map { _.getMessage }

    def to[E[_]: Eff: WrapErr]: E[B] = x match {
      case Right(r) => Eff.ok(r)
      case Left(e)  => Eff.ko(e)
    }
  }

  implicit class EffLeftErrOps[B](val x: Either[Err, B]) extends AnyVal {

    def to[E[_]: Eff: WrapErr]: E[B] = x match {
      case Right(r) => Eff.ok(r)
      case Left(e)  => Eff.ko(e)
    }
  }

  implicit class EffEitherObjOps(private val x: Either.type) extends AnyVal {

    def eval[T >: Null <: Throwable]: EvalEither[T] = new EvalEither[T]
  }

  implicit class EffTryOps[T](private val x: Try[T]) extends AnyVal {

    def to[E[_]: Eff: WrapErr]: E[T] = x match {
      case Success(r) => Eff.ok(r)
      case Failure(e) => Eff.ko(e)
    }

    def getOrThrow: T = x.get
  }

  implicit class EffBooleanOps(private val t: Boolean) extends AnyVal {

    def orLeft[T](x: T): Either[T, Boolean] = if (!t) Left(x) else Right(t)
  }

  implicit class EffAnyTypedOps[T](private val x: T) extends AnyVal {

    def ok[E[_]: Eff: WrapErr]: E[T] = Eff.ok(x)
  }

  implicit class EffErrKindOps(private val x: ErrKind) extends AnyVal {

    def err[E[_]: WrapErr, R]: E[R] = WrapErr[E].apply[R](x)
  }

  implicit class EffFunctorOps[E[_], R](private val x: E[R]) extends AnyVal {

    def map[RR](fn: R => RR)(implicit f: Functor[E]): E[RR] = f.map(x)(fn)

    def flatMap[RR](fn: R => E[RR])(implicit f: Functor[E]): E[RR] = f.flatMap(x)(fn)
  }

  implicit class EffOptionOps[T](private val x: Option[T]) extends AnyVal {

    def to[E[_]: Eff: WrapErr](alt: => ErrKind): E[T] = x match {
      case Some(r) => Eff.ok(r)
      case None    => Eff.ko(alt)
    }

    def getOrThrow(implicit TH: Throw[String]): T = x.fold(TH("empty"))(identity)
  }
}
