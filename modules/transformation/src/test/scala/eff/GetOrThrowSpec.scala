package eff

import org.scalatest.wordspec._
import org.scalatest.matchers.should.Matchers._

import scala.util._


class GetOrThrowSpec extends AnyWordSpec {

  "getOrThrow" should {

    "be accessible" when {

      "Result" in {
        Ok(12).getOrThrow shouldBe 12
        intercept[ThrowableErr] { Err.Msg("fail").getOrThrow }.value shouldBe Err.Msg("fail")
        intercept[TestException] { Err.Exception(TestException("fail")).getOrThrow }.value shouldBe "fail"
      }

      "Option" in {
        Some(12).getOrThrow shouldBe 12
        intercept[ThrowableErr] { None.getOrThrow }.value shouldBe Err.Msg("empty")
      }

      "Try" in {
        Success(12).getOrThrow shouldBe 12
        intercept[TestException] { Failure(TestException("fail")).getOrThrow }.value shouldBe "fail"
      }

      "Either[String, *]" in {
        Right[String, Int](12).getOrThrow shouldBe 12
        intercept[ThrowableErr] { Left[String, Int]("fail").getOrThrow }.value shouldBe Err.Msg("fail")
      }

      "Either[Throwable, *]" in {
        Right[Throwable, Int](12).getOrThrow shouldBe 12
        intercept[TestException] { Left[Throwable, Int](TestException("fail")).getOrThrow }.value shouldBe "fail"
      }

      "Either[Err, *]" in {
        Right[Err, Int](12).getOrThrow shouldBe 12
        intercept[ThrowableErr] { Left[Err, Int](Err.Msg("fail")).getOrThrow }.value shouldBe Err.Msg("fail")
        intercept[TestException] { Left[Err, Int](Err.Exception(TestException("fail"))).getOrThrow }.value shouldBe "fail"
      }
    }
  }
}
