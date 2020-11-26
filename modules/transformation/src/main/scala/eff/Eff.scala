package eff

import scala.util._


sealed abstract class Eff[E[_]] {
  import Eff._

  type KO[R] <: E[R]

  type OK[R] <: E[R]

  def KO[R]: KoView[E, KO, R]

  def OK[R]: OkView[E, R]

  def ok[R](x: R): E[R] = OK(x)

  def ko[R](err: ErrKind)(implicit wrap: WrapErr[E]): E[R] = wrap[R](err)

  def mergeKos[R](l: KO[R], r: KO[R]): KO[R]
}

object Eff {

  def apply[E[_]: Eff]: Eff[E] = implicitly

  def ok[E[_]: Eff, R](x: R): E[R] = Eff[E].ok(x)

  def ko[E[_]: Eff, R](x: ErrKind)(implicit wrap: WrapErr[E]): E[R] = Eff[E].ko[R](x)

  trait KoView[E[_], L[_], R] {
    def unapply(x: E[R]): Option[L[R]]
  }

  trait OkView[E[_], R] {
    def apply(x: R): E[R]
    def unapply(x: E[R]): Option[R]
  }

  implicit val opaqueEffectType: Eff[Opaque] = new Eff[Opaque] {
    override type KO[R] = Nothing
    override type OK[R] = R

    override def KO[R]: KoView[Opaque, KO, R] = new KoView[Opaque, KO, R] {
      override def unapply(x: Opaque[R]): Option[KO[R]] = None
    }

    override def OK[R]: OkView[Opaque, R] = new OkView[Opaque, R] {
      override def apply(x: R): Opaque[R] = x
      override def unapply(x: Opaque[R]): Option[R] = Some(x)
    }

    override def mergeKos[R](l: KO[R], r: KO[R]): KO[R] = ().asInstanceOf[Nothing]
  }

  implicit val optionEffectType: Eff[Option] = new Eff[Option] {
    override type KO[R] = None.type
    override type OK[R] = Some[R]

    override def KO[R]: KoView[Option, KO, R] = new KoView[Option, KO, R] {
      override def unapply(x: Option[R]): Option[KO[R]] = x match {
        case None => Some(None)
        case _    => None
      }
    }

    override def OK[R]: OkView[Option, R] = new OkView[Option, R] {
      override def apply(x: R): Option[R] = Option(x)
      override def unapply(x: Option[R]): Option[R] = x
    }

    override def mergeKos[R](l: KO[R], r: KO[R]): KO[R] = None
  }

  implicit val tryEffectType: Eff[Try] = new Eff[Try] {
    override type KO[R] = Failure[R]
    override type OK[R] = Success[R]

    override def KO[R]: KoView[Try, KO, R] = new KoView[Try, KO, R] {
      override def unapply(x: Try[R]): Option[KO[R]] = x match {
        case x: Failure[_] => Some(x.asInstanceOf[KO[R]])
        case _             => None
      }
    }

    override def OK[R]: OkView[Try, R] = new OkView[Try, R] {
      override def apply(x: R): Try[R] = Success(x)
      override def unapply(x: Try[R]): Option[R] = x.toOption
    }

    override def mergeKos[R](l: KO[R], r: KO[R]): KO[R] = {
      l.exception.addSuppressed(r.exception)
      l
    }
  }

  implicit val resultEffectType: Eff[Result] = new Eff[Result] {
    override type KO[R] = Err
    override type OK[R] = Ok[R]

    override def KO[R]: KoView[Result, KO, R] = new KoView[Result, KO, R] {
      override def unapply(x: Result[R]): Option[KO[R]] = x match {
        case x: Err => Some(x.asInstanceOf[KO[R]])
        case _      => None
      }
    }

    override def OK[R]: OkView[Result, R] = new OkView[Result, R] {
      override def apply(x: R): Result[R] = Ok(x)
      override def unapply(x: Result[R]): Option[R] = x.toOption
    }

    override def mergeKos[R](l: KO[R], r: KO[R]): KO[R] = l merge r
  }

  implicit def eitherEffectType[L](implicit
    wrap: WrapErr[({ type E[T] = Either[L, T] })#E],
    mergeL: Merge[L]): Eff[({ type E[T] = Either[L, T] })#E] = {

    new Eff[({ type E[T] = Either[L, T] })#E] {
      override type KO[R] = scala.Left[L, R]
      override type OK[R] = scala.Right[L, R]

      override def KO[R]: KoView[({ type E[T] = Either[L, T] })#E, KO, R] = new KoView[({ type E[T] = Either[L, T] })#E, KO, R] {
        override def unapply(x: Either[L, R]): Option[KO[R]] = x match {
          case x: KO[_] => Some(x.asInstanceOf[KO[R]])
          case _        => None
        }
      }

      override def OK[R]: OkView[({ type E[T] = Either[L, T] })#E, R] = new OkView[({ type E[T] = Either[L, T] })#E, R] {
        override def apply(x: R): Either[L, R] = scala.Right(x)
        override def unapply(x: Either[L, R]): Option[R] = x.toOption
      }

      override def mergeKos[R](l: KO[R], r: KO[R]): KO[R] = {
        val (Left(a), Left(b)) = (l, r)
        scala.Left(mergeL(a, b))
      }
    }
  }
}