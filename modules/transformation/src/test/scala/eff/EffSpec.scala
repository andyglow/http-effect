package eff

import org.scalatest.wordspec._
import org.scalatest.matchers.should.Matchers._

import scala.util._

class EffSpec extends AnyWordSpec {
  import EffSpec._

  "Eff" when {

    "Option" should {

      "instantiate" in {
        //    "implicitly[EffectCompanion[Option].Left[Int] =:= None]" should compile
        //    "implicitly[EffectCompanion[Option].Left[Int] =:= Some[Int]]" should compile
        Eff[Option].ok("foo") shouldBe Some("foo")
        Eff[Option].ko("foo") shouldBe None
        Eff[Option].ko(ThrowableErr("foo")) shouldBe None
      }

      "convert to Result" in {
        Some(20).to[Result]("err") shouldBe Ok(20)
        None.to[Result]("err") shouldBe Err.Msg("err")
        None.to[Result](Err.Msg("err")) shouldBe Err.Msg("err")
        None.to[Result](TestException("err")) shouldBe Err.Exception(TestException("err"))
      }

      "convert to Either[String, ?]" in {
        Some(20).to[StringOr]("err") shouldBe Right(20)
        None.to[StringOr]("err") shouldBe Left("err")
      }

      "convert to Either[Throwable, ?]" in {
        Some(20).to[ThrowableOr]("err") shouldBe Right(20)
        None.to[ThrowableOr](ThrowableErr("err")) shouldBe Left(ThrowableErr("err"))
        None.to[ThrowableOr](TestException("err")) shouldBe Left(TestException("err"))
        None.to[ThrowableOr]("err") shouldBe Left(ThrowableErr("err"))
      }

      "convert to Either[Err, ?]" in {
        Some(20).to[ErrOr]("err") shouldBe Right(20)
        None.to[ErrOr]("err") shouldBe Left(Err.Msg("err"))
        None.to[ErrOr](TestException("err")) shouldBe Left(Err.Exception(TestException("err")))
      }

      "convert to Try" in {
        Some(20).to[Try]("err") shouldBe Success(20)
        None.to[Try]("err") shouldBe Failure(ThrowableErr("err"))
        None.to[Try](TestException("err")) shouldBe Failure(TestException("err"))
      }
    }

    "Try" should {

      "instantiate" in {
        Eff[Try].ok("foo") shouldBe Success("foo")
        Eff[Try].ko("foo") shouldBe Failure(ThrowableErr("foo"))
        Eff[Try].ko(ThrowableErr("foo")) shouldBe Failure(ThrowableErr("foo"))
      }

      "convert to Result" in {
        Success(20).to[Result] shouldBe Ok(20)
        Failure[Int](TestException("err")).to[Result] shouldBe Err.Exception(TestException("err"))
      }

      "convert to Option" in {
        Success(20).to[Option] shouldBe Some(20)
        Failure[Int](TestException("err")).to[Option] shouldBe None
      }

      "convert to Either[String, ?]" in {
        Success(20).to[StringOr] shouldBe Right(20)
        Failure[Int](TestException("err")).to[StringOr] shouldBe Left("eff.TestException")
        Failure[Int](ThrowableErr("err")).to[StringOr] shouldBe Left("err")
      }

      "convert to Either[Throwable, ?]" in {
        Success(20).to[ThrowableOr] shouldBe Right(20)
        Failure[Int](TestException("err")).to[ThrowableOr] shouldBe Left(TestException("err"))
        Failure[Int](ThrowableErr("err")).to[ThrowableOr] shouldBe Left(ThrowableErr("err"))
      }

      "convert to Either[Err, ?]" in {
        Success(20).to[ErrOr] shouldBe Right(20)
        Failure[Int](TestException("err")).to[ErrOr] shouldBe Left(Err.Exception(TestException("err")))
        Failure[Int](ThrowableErr("err")).to[ErrOr] shouldBe Left(Err.Msg("err"))
      }
    }

    "Result" should {

      "instantiate" in {
        Eff[Result].ok("foo") shouldBe Ok("foo")
        Eff[Result].ko("foo") shouldBe Err.Msg("foo")
        Eff[Result].ko(ThrowableErr("foo")) shouldBe Err.Msg("foo")
        Eff[Result].ko(TestException("foo")) shouldBe Err.Exception(TestException("foo"))
      }

      "convert to Try" in {
        Ok(20).to[Try] shouldBe Success(20)
        Err.Msg("foo").to[Try] shouldBe Failure(ThrowableErr("foo"))
        Err.Exception(TestException("foo")).to[Try] shouldBe Failure(TestException("foo"))
      }

      "convert to Option" in {
        Ok(20).to[Option] shouldBe Some(20)
        Err.Msg("foo").to[Option] shouldBe None
        Err.Exception(TestException("foo")).to[Option] shouldBe None
      }

      "convert to Either[String, ?]" in {
        Ok(20).to[StringOr] shouldBe Right(20)
        Err.Msg("foo").to[StringOr] shouldBe Left("foo")
        Err.Exception(TestException("foo")).to[StringOr] shouldBe Left("eff.TestException")
        Err.Exception(ThrowableErr("foo")).to[StringOr] shouldBe Left("eff.ThrowableErr: foo")
      }

      "convert to Either[Throwable, ?]" in {
        Ok(20).to[ThrowableOr] shouldBe Right(20)
        Err.Msg("foo").to[ThrowableOr] shouldBe Left(ThrowableErr("foo"))
        Err.Exception(TestException("foo")).to[ThrowableOr] shouldBe Left(TestException("foo"))
        Err.Exception(ThrowableErr("foo")).to[ThrowableOr] shouldBe Left(ThrowableErr("foo"))
      }

      "convert to Either[Err, ?]" in {
        Ok(20).to[ErrOr] shouldBe Right(20)
        Err.Msg("foo").to[ErrOr] shouldBe Left(Err.Msg("foo"))
        Err.Exception(TestException("foo")).to[ErrOr] shouldBe Left(Err.Exception(TestException("foo")))
        Err.Exception(ThrowableErr("foo")).to[ErrOr] shouldBe Left(Err.Exception(ThrowableErr("foo")))
      }
    }

    "Either[String, *]" should {

      "instantiate" in {
        Eff[StringOr].ok("foo") shouldBe Right("foo")
        Eff[StringOr].ko("foo") shouldBe Left("foo")
        Eff[StringOr].ko(TestException("foo")) shouldBe Left("eff.TestException")
        Eff[StringOr].ko(ThrowableErr("foo")) shouldBe Left("foo")
      }

      "convert to Result" in {
        Right[String, Int](20).to[Result] shouldBe Ok(20)
        Left[String, Int]("foo").to[Result] shouldBe Err.Msg("foo")
      }

      "convert to Try" in {
        Right[String, Int](20).to[Try] shouldBe Success(20)
        Left[String, Int]("foo").to[Try] shouldBe Failure(ThrowableErr("foo"))
      }

      "convert to Option" in {
        Right[String, Int](20).to[Option] shouldBe Some(20)
        Left[String, Int]("foo").to[Option] shouldBe None
      }

      "convert to Either[Throwable, ?]" in {
        Right[String, Int](20).to[ThrowableOr] shouldBe Right(20)
        Left[String, Int]("foo").to[ThrowableOr] shouldBe Left(ThrowableErr("foo"))
      }

      "convert to Either[Err, ?]" in {
        Right[String, Int](20).to[ErrOr] shouldBe Right(20)
        Left[String, Int]("foo").to[ErrOr] shouldBe Left(Err.Msg("foo"))
      }
    }

    "Either[Throwable, *]" should {

      "instantiate" in {
        Eff[ThrowableOr].ok("foo") shouldBe Right("foo")
        Eff[ThrowableOr].ko("foo") shouldBe Left(ThrowableErr("foo"))
        Eff[ThrowableOr].ko(ThrowableErr("foo")) shouldBe Left(ThrowableErr("foo"))
      }

      "convert to Result" in {
        Right[Throwable, Int](20).to[Result] shouldBe Ok(20)
        Left[Throwable, Int](ThrowableErr("foo")).to[Result] shouldBe Err.Msg("foo")
        Left[Throwable, Int](TestException("foo")).to[Result] shouldBe Err.Exception(TestException("foo"))
      }

      "convert to Try" in {
        Right[Throwable, Int](20).to[Try] shouldBe Success(20)
        Left[Throwable, Int](ThrowableErr("foo")).to[Try] shouldBe Failure(ThrowableErr("foo"))
        Left[Throwable, Int](TestException("foo")).to[Try] shouldBe Failure(TestException("foo"))
      }

      "convert to Option" in {
        Right[Throwable, Int](20).to[Option] shouldBe Some(20)
        Left[Throwable, Int](ThrowableErr("foo")).to[Option] shouldBe None
        Left[Throwable, Int](TestException("foo")).to[Option] shouldBe None
      }

      "convert to Either[String, ?]" in {
        Right[Throwable, Int](20).to[StringOr] shouldBe Right(20)
        Left[Throwable, Int](ThrowableErr("foo")).to[StringOr] shouldBe Left("foo")
        Left[Throwable, Int](TestException("foo")).to[StringOr] shouldBe Left("eff.TestException")
      }

      "convert to Either[Err, ?]" in {
        Right[Throwable, Int](20).to[ErrOr] shouldBe Right(20)
        Left[Throwable, Int](ThrowableErr("foo")).to[ErrOr] shouldBe Left(Err.Msg("foo"))
        Left[Throwable, Int](TestException("foo")).to[ErrOr] shouldBe Left(Err.Exception(TestException("foo")))
      }
    }

    "Either[Err, *]" should {

      "instantiate" in {
        implicitly[WrapErr[ErrOr]]
        Eff[ErrOr].ok("foo") shouldBe Right("foo")
        Eff[ErrOr].ko("foo") shouldBe Left(Err.Msg("foo"))
        Eff[ErrOr].ko(ThrowableErr("foo")) shouldBe Left(Err.Msg("foo"))
        Eff[ErrOr].ko(TestException("foo")) shouldBe Left(Err.Exception(TestException("foo")))
      }

      "convert to Result" in {
        Right[Err, Int](20).to[Result] shouldBe Ok(20)
        Left[Err, Int](Err.Msg("foo")).to[Result] shouldBe Err.Msg("foo")
        Left[Err, Int](Err.Exception(TestException("foo"))).to[Result] shouldBe Err.Exception(TestException("foo"))
      }

      "convert to Try" in {
        Right[Err, Int](20).to[Try] shouldBe Success(20)
        Left[Err, Int](Err.Msg("foo")).to[Try] shouldBe Failure(ThrowableErr("foo"))
        Left[Err, Int](Err.Exception(TestException("foo"))).to[Try] shouldBe Failure(TestException("foo"))
      }

      "convert to Option" in {
        Right[Err, Int](20).to[Option] shouldBe Some(20)
        Left[Err, Int](Err.Msg("foo")).to[Option] shouldBe None
        Left[Err, Int](Err.Exception(TestException("foo"))).to[Option] shouldBe None
      }

      "convert to Either[Throwable, ?]" in {
        Right[Err, Int](20).to[ThrowableOr] shouldBe Right(20)
        Left[Err, Int](Err.Msg("foo")).to[ThrowableOr] shouldBe Left(ThrowableErr("foo"))
        Left[Err, Int](Err.Exception(TestException("foo"))).to[ThrowableOr] shouldBe Left(TestException("foo"))
      }

      "convert to Either[String, ?]" in {
        Right[Err, Int](20).to[StringOr] shouldBe Right(20)
        Left[Err, Int](Err.Msg("foo")).to[StringOr] shouldBe Left("foo")
        Left[Err, Int](Err.Exception(TestException("foo"))).to[StringOr] shouldBe Left("eff.TestException")
      }
    }
  }
}

object EffSpec {
  private type ErrOr[T] = Either[Err, T]
  private type StringOr[T] = Either[String, T]
  private type ThrowableOr[T] = Either[Throwable, T]
}