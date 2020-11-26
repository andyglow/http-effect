package eff

import org.scalatest.funsuite._
import org.scalatest.matchers.should.Matchers._


class EitherOpsSpec extends AnyFunSuite {
  import EitherOpsSpec._

  test("filterOrElse") {
    ok.filterOrElse(_ == 10, "loss") shouldBe ok
    ok.filterOrElse(_ != 10, "loss") shouldBe Left("loss")
    er.filterOrElse(_ == 10, "loss") shouldBe er
  }

  test("otherwise") {
    ok.otherwise(_ => Right(true)) shouldBe ok
    er.otherwise(_ => Right(true)) shouldBe Right(true)
    er.otherwise(err => Left(s"failed: $err")) shouldBe Left("failed: error")
  }

  test(":>") {
    (er :> Right(14)) shouldBe Right(14)
    (ok :> Right(14)) shouldBe ok
  }

  test("getOrThrow") {
    ok.getOrThrow shouldBe 10
    an[ThrowableErr] shouldBe thrownBy { er.getOrThrow }
    an[RuntimeException] shouldBe thrownBy {
      er.getOrThrow(Throw(new RuntimeException(_)))
    }
  }
}

object EitherOpsSpec {

  val ok: Either[String, Int] = Right(10)
  val er: Either[String, Int] = Left("error")
}
