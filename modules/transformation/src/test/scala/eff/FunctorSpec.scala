package eff

import scala.util._
import org.scalatest.wordspec._
import org.scalatest.matchers.should.Matchers._


class FunctorSpec extends AnyWordSpec {
  import FunctorSpec._

  "Functor[Option]" should {
    "compile" in { "Functor[Option]" should compile }
    "map" in {
      Functor[Option].map(Some(a))(mapA) shouldBe Some(ab)
      Functor[Option].map(None   )(mapA) shouldBe None
    }
    "flatMap" in {
      Functor[Option].flatMap(Some(a))(flatMapA[Option])   shouldBe Some(ab)
      Functor[Option].flatMap(Some(a))(flatMapErr[Option]) shouldBe None
      Functor[Option].flatMap(None   )(flatMapA[Option])   shouldBe None
      Functor[Option].flatMap(None   )(flatMapErr[Option]) shouldBe None
    }
  }

  "Functor[Try]" should {
    val fail = Failure(ThrowableErr("fail"))
    "compile" in { "Functor[Try]" should compile }
    "map" in {
      Functor[Try].map(Success(a))(mapA) shouldBe Success(ab)
      Functor[Try].map(fail      )(mapA) shouldBe fail
    }
    "flatMap" in {
      Functor[Try].flatMap(Success(a))(flatMapA[Try])   shouldBe Success(ab)
      Functor[Try].flatMap(Success(a))(flatMapErr[Try]) shouldBe Failure(ThrowableErr("error: a"))
      Functor[Try].flatMap(fail      )(flatMapA[Try])   shouldBe fail
      Functor[Try].flatMap(fail      )(flatMapErr[Try]) shouldBe fail
    }
  }

  "Functor[Result]" should {
    val fail = Err.Msg("fail")
    "compile" in { "Functor[Result]" should compile }
    "map" in {
      Functor[Result].map(Ok(a))(mapA) shouldBe Ok(ab)
      Functor[Result].map(fail )(mapA) shouldBe fail
    }
    "flatMap" in {
      Functor[Result].flatMap(Ok(a))(flatMapA[Result])   shouldBe Ok(ab)
      Functor[Result].flatMap(Ok(a))(flatMapErr[Result]) shouldBe Err.Msg("error: a")
      Functor[Result].flatMap(fail )(flatMapA[Result])   shouldBe fail
      Functor[Result].flatMap(fail )(flatMapErr[Result]) shouldBe fail
    }
  }

  "Functor[Either[String, *]]" should {
    val fail = Left[String, String]("fail")
    "compile" in { "type ErrOr[T] = Either[String, T]; Functor[ErrOr]" should compile }
    "map" in {
      Functor[StringOr].map(Right(a))(mapA) shouldBe Right(ab)
      Functor[StringOr].map(fail    )(mapA) shouldBe fail
    }
    "flatMap" in {
      Functor[StringOr].flatMap(Right(a))(flatMapA[StringOr])   shouldBe Right(ab)
      Functor[StringOr].flatMap(Right(a))(flatMapErr[StringOr]) shouldBe Left("error: a")
      Functor[StringOr].flatMap(fail    )(flatMapA[StringOr])   shouldBe fail
      Functor[StringOr].flatMap(fail    )(flatMapErr[StringOr]) shouldBe fail
    }
  }

  "Functor[Either[Throwable, *]]" should {
    val fail = Left[Throwable, String](ThrowableErr("fail"))
    "compile" in { "type ErrOr[T] = Either[String, T]; Functor[ErrOr]" should compile }
    "map" in {
      Functor[ThrowableOr].map(Right(a))(mapA) shouldBe Right(ab)
      Functor[ThrowableOr].map(fail    )(mapA) shouldBe fail
    }
    "flatMap" in {
      Functor[ThrowableOr].flatMap(Right(a))(flatMapA[ThrowableOr])   shouldBe Right(ab)
      Functor[ThrowableOr].flatMap(Right(a))(flatMapErr[ThrowableOr]) shouldBe Left(ThrowableErr("error: a"))
      Functor[ThrowableOr].flatMap(fail    )(flatMapA[ThrowableOr])   shouldBe fail
      Functor[ThrowableOr].flatMap(fail    )(flatMapErr[ThrowableOr]) shouldBe fail
    }
  }
}

object FunctorSpec {
  private type StringOr[T] = Either[String, T]
  private type ThrowableOr[T] = Either[Throwable, T]

  private val a = "a"
  private val ab = "ab"
  private def mapA(a: String): String = a + "b"
  private def flatMapA[E[_]](a: String)(implicit eff: Eff[E]): E[String] = eff.ok(a + "b")
  private def flatMapErr[E[_]: Eff: WrapErr](a: String): E[String] = Eff.ko(s"error: $a")
}