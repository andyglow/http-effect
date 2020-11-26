package eff

import org.scalatest.funsuite._
import org.scalatest.matchers.should.Matchers._


class ResultSpec extends AnyFunSuite {
  import ResultSpec._

  test("isSuccess") {
    Ok(a).isSuccess shouldBe true
    errM.isSuccess  shouldBe false
    errS.isSuccess  shouldBe false
  }

  test("isFailure") {
    Ok(a).isFailure shouldBe false
    errM.isFailure  shouldBe true
    errS.isFailure  shouldBe true
  }

  test("map") {
    Ok(a) map mapA shouldBe Ok(ab)
    errM map mapA shouldBe errM
    errS map mapA shouldBe errS
  }

  test("flatMap") {
    Ok(a) flatMap flatMapA shouldBe Ok(ab)
    errM flatMap flatMapA shouldBe errM
    errS flatMap flatMapA shouldBe errS

    Ok(a) flatMap flatMapErr shouldBe Err.Msg("error: a")
    errM flatMap flatMapErr shouldBe errM
    errS flatMap flatMapErr shouldBe errS
  }

  test("orElse") {
    Ok(a) orElse Ok(ab) shouldBe Ok(a)
    errM orElse Ok(ab) shouldBe Ok(ab)
    errS orElse Ok(ab) shouldBe Ok(ab)

    errM orElse errS shouldBe errS
    errS orElse errM shouldBe errM
  }

  test("getOrElse") {
    Ok(a) getOrElse ab shouldBe a
    errM getOrElse ab shouldBe ab
    errS getOrElse ab shouldBe ab
  }

  test("foreach") {
    var v: Option[String] = None
    Ok(a) foreach { x => v = Some(x) }
    v match {
      case Some(`a`) =>
      case Some(_)   => fail("Ok.foreach has been invoked, but the value looks weird")
      case None      => fail("Ok.foreach hasn't been invoked")
    }

    errM foreach { _ => fail("Err.foreach shouldn't be invoked") }
    errS foreach { _ => fail("Err.foreach shouldn't be invoked") }
  }

  test("collect") {
    Ok(a) collect { case _ => ab } shouldBe Ok(ab)
    Ok(a) collect { case `a` => ab } shouldBe Ok(ab)
    Ok(a) collect { case `ab` => ab } shouldBe Err.Exception(Err.NotDefined)

    errM collect { case _ => ab } shouldBe errM
    errM collect { case `a` => ab } shouldBe errM
    errM collect { case `ab` => ab } shouldBe errM

    errS collect { case _ => ab } shouldBe errS
    errS collect { case `a` => ab } shouldBe errS
    errS collect { case `ab` => ab } shouldBe errS
  }

  test("filter") {
    Ok(a).filter(_ == a) shouldBe Ok(a)
    Ok(a).filter(_ != a) shouldBe Err.Exception(Err.NotDefined)

    errM.filter(_ == a) shouldBe errM
    errM.filter(_ != a) shouldBe errM

    errS.filter(_ == a) shouldBe errS
    errS.filter(_ != a) shouldBe errS
  }

  test("toOption") {
    Ok(a).toOption shouldBe Some(a)
    errM.toOption shouldBe None
    errS.toOption shouldBe None
  }

  test("toEither") {
    Ok(a).toEither shouldBe Right(a)
    errM.toEither shouldBe Left(errM)
    errS.toEither shouldBe Left(errS)
  }

  test("fold") {
    Ok(a).fold(_ => 0, _ => 1) shouldBe 1
    errM.fold(_ => 0, _ => 1) shouldBe 0
    errS.fold(_ => 0, _ => 1) shouldBe 0
  }

  test("transform") {
    Ok(a).transform(a => Ok(a + "z"), _ => Ok("x")) shouldBe Ok("az")
    errM.transform(a => Ok(a + "z"), _ => Ok("x")) shouldBe Ok("x")
    errS.transform(a => Ok(a + "z"), _ => Ok("x")) shouldBe Ok("x")
  }

  test("recover") {
    Ok(a) recover { case _: Err => ab } shouldBe Ok(a)
    errM recover { case _: Err => throw ThrowableErr("fail") } shouldBe Err.Exception(ThrowableErr("fail"))
    errM recover { case _: Err => ab } shouldBe Ok(ab)
    errS recover { case _: Err => ab } shouldBe Ok(ab)
  }

  test("recoverWith") {
    Ok(a) recoverWith { case _: Err => Ok(ab) } shouldBe Ok(a)
    errM recoverWith { case _: Err => Err.Exception(ThrowableErr("fail")) } shouldBe Err.Exception(ThrowableErr("fail"))
    errM recoverWith { case _: Err => Ok(ab) } shouldBe Ok(ab)
    errS recoverWith { case _: Err => Ok(ab) } shouldBe Ok(ab)
  }

  test("flatten") {
    Ok(Ok(a)).flatten shouldBe Ok(a)
  }
//
//  test("getOrThrow") {
//    Ok(a).getOrThrow shouldBe a
//    intercept[ThrowableErr](errM.getOrThrow).getMessage shouldBe "fail"
//    intercept[ThrowableErr](errS.getOrThrow).getMessage shouldBe "fail"
//  }

  import Err._

  test("Err :+") {
    Msg("fail") :+ Msg("reason1") :+ Msg("reason2") shouldBe Err("fail", Msg("reason1"), Msg("reason2"))
    Msg("fail") :+ "reason1" :+ "reason2" shouldBe Err("fail", Msg("reason1"), Msg("reason2"))
    Msg("fail") :+ ThrowableErr("reason1") :+ ThrowableErr("reason2") shouldBe Err("fail", Exception(ThrowableErr("reason1")), Exception(ThrowableErr("reason2")))
  }

  test("Err +:") {
    Msg("reason1") +: Msg("reason2") +: Msg("fail") shouldBe Err("fail", Msg("reason1"), Msg("reason2"))
    "reason1" +: "reason2" +: Msg("fail") shouldBe Err("fail", Msg("reason1"), Msg("reason2"))
    ThrowableErr("reason1") +: ThrowableErr("reason2") +: Msg("fail") shouldBe Err("fail", Exception(ThrowableErr("reason1")), Exception(ThrowableErr("reason2")))
  }

  test("Err.merge") {
    Msg("err1") merge Msg("err2") shouldBe ErrList(List(Msg("err1"), Msg("err2")))

    import Conversions._

    "err1" merge ThrowableErr("err2") shouldBe ErrList(List(Msg("err1"), Exception(ThrowableErr("err2"))))
  }

  test("Err.Conversions") {
    import Conversions._

    "err1" merge ThrowableErr("err2") shouldBe ErrList(List(Msg("err1"), Exception(ThrowableErr("err2"))))
  }

  test("Err.Converters") {

    "err1".asErr merge ThrowableErr("err2").asErr shouldBe ErrList(List(Msg("err1"), Exception(ThrowableErr("err2"))))
  }

  test("Err.message") {
    Err("error").message shouldBe "error"
    Err("error line1\nerror line2").message shouldBe "error line1\nerror line2"
    Err("error", Msg("reason1")).message shouldBe "error\n  - reason1"
    Err("error", Msg("reason1"), Msg("reason2")).message shouldBe "error\n  - reason1\n  - reason2"

    Exception(ThrowableErr("error")).message shouldBe "eff.ThrowableErr: error\n"

    val ex = new java.lang.Exception("error")
    ex.setStackTrace(Array(
      new StackTraceElement("a", "b", "c", 5),
      new StackTraceElement("a", "b", "c", 8),
      new StackTraceElement("a", "b", "c", 10)))

    Exception(ex).message shouldBe
      """java.lang.Exception: error
        |	at a.b(c:5)
        |	at a.b(c:8)
        |	at a.b(c:10)
        |""".stripMargin

    Err("error", Exception(ex)).message shouldBe
      """error
        |  java.lang.Exception: error
        |  	at a.b(c:5)
        |  	at a.b(c:8)
        |  	at a.b(c:10)
        |""".stripMargin
  }
}

object ResultSpec {

  private val a = "a"
  private val ab = "ab"
  private val errM: Result[String] = Err.Msg("fail")
  private val errS: Result[String] = Err.Exception(ThrowableErr("fail"))
  private def mapA(a: String): String = a + "b"
  private def flatMapA(a: String): Result[String] = Ok(a + "b")
  private def flatMapErr(a: String): Result[String] = Err.Msg(s"error: $a")
}