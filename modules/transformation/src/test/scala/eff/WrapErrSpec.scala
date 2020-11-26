package eff

import org.scalatest.funsuite._
import org.scalatest.matchers.should.Matchers._

import scala.util._


class WrapErrSpec extends AnyFunSuite {
  import WrapErrSpec._

  test("Option") {
    mk[Option, String]("foo") shouldBe None
    mk[Option, String](new Exception("foo")) shouldBe None
  }

  test("Result") {
    mk[Result, String]("foo") shouldBe Err.Msg("foo")
    mk[Result, String](ThrowableErr("foo")) shouldBe Err.Exception(ThrowableErr("foo"))
    mk[Result, String](TestException("foo")) shouldBe Err.Exception(TestException("foo"))
  }

  test("Try") {
    mk[Try, String]("foo") shouldBe Failure(ThrowableErr("foo"))
    mk[Try, String](ThrowableErr("foo")) shouldBe Failure(ThrowableErr("foo"))
  }

  test("Either") {
    mk[StringOr, String]("foo") shouldBe Left("foo")
    mk[StringOr, String](ThrowableErr("foo")) shouldBe Left("eff.ThrowableErr: foo")
    mk[ErrOr, String]("foo") shouldBe Left(Err.Msg("foo"))
    mk[ErrOr, String](ThrowableErr("foo")) shouldBe Left(Err.Exception(ThrowableErr("foo")))
    mk[ThrowableOr, String]("foo") shouldBe Left(ThrowableErr("foo"))
    mk[ThrowableOr, String](ThrowableErr("foo")) shouldBe Left(ThrowableErr("foo"))
  }
}

object WrapErrSpec {
  def mk[E[_]: WrapErr, T](x: ErrKind): E[T] = WrapErr[E, T](x)

  private type ErrOr[T] = Either[Err, T]
  private type StringOr[T] = Either[String, T]
  private type ThrowableOr[T] = Either[Throwable, T]

}