package eff

import java.io.{PrintWriter, StringWriter}

import scala.language.implicitConversions
import scala.util.control.{NoStackTrace, NonFatal}

/** It is a form of Either with 2 types allowed on left: String or Throwable
  *
  * 3 constructions allowed
  * - {{{Ok(val)}}} for positive result
  * - {{{Err.Message(msg)}}} for negative result represented as a string
  * - {{{Err.Stack(ex)}}} for negative result represented as throwable
  *
  * @tparam T Value Type
  */
sealed trait Result[+T] extends Product with Serializable {

  def isSuccess: Boolean

  def isFailure: Boolean = !isSuccess

  def map[TT](fn: T => TT): Result[TT]

  def flatMap[TT](fn: T => Result[TT]): Result[TT]

  def orElse[U >: T](default: => Result[U]): Result[U] = if (isSuccess) this else default

  def getOrElse[U >: T](default: => U): U

  def foreach[U](f: T => U): Unit

  def collect[U](pf: PartialFunction[T, U]): Result[U]

  def filter(p: T => Boolean): Result[T]

  @inline final def withFilter(p: T => Boolean): WithFilter = new WithFilter(p)

  class WithFilter(p: T => Boolean) {
    def map[U](f:     T => U): Result[U]          = Result.this filter p map f
    def flatMap[U](f: T => Result[U]): Result[U]  = Result.this filter p flatMap f
    def foreach[U](f: T => U): Unit               = Result.this filter p foreach f
    def withFilter(q: T => Boolean): WithFilter   = new WithFilter(x => p(x) && q(x))
  }

  def toOption: Option[T]

  def toEither: Either[Err, T]

  def fold[U](fa: Err => U, fb: T => U): U

  def transform[U](s: T => Result[U], f: Err => Result[U]): Result[U]

  def recover[U >: T](pf: PartialFunction[Err, U]): Result[U]

  def recoverWith[U >: T](pf: PartialFunction[Err, Result[U]]): Result[U]

  def flatten[U](implicit ev: T <:< Result[U]): Result[U]
}

case class Ok[+T](value: T) extends Result[T] {
  import Err._

  def isSuccess: Boolean = true

  def map[TT](fn: T => TT): Result[TT] = Ok(fn(value))

  def flatMap[TT](fn: T => Result[TT]): Result[TT] = fn(value)

  def getOrElse[U >: T](default: => U): U = value

  def foreach[U](f: T => U): Unit = f(value)

  def collect[U](pf: PartialFunction[T, U]): Result[U] = if (pf.isDefinedAt(value)) Ok(pf(value)) else Err.Exception(NotDefined)

  def filter(p: T => Boolean): Result[T] = if (p(value)) this else Err.Exception(NotDefined)

  def toOption: Option[T] = Some(value)

  def toEither: Either[Err, T] = Right(value)

  def fold[U](fa: Err => U, fb: T => U): U = fb(value)

  def transform[U](s: T => Result[U], f: Err => Result[U]): Result[U] = s(value)

  def recover[U >: T](pf: PartialFunction[Err, U]): Result[U] = this

  def recoverWith[U >: T](pf: PartialFunction[Err, Result[U]]): Result[U] = this

  def flatten[U](implicit ev: T <:< Result[U]): Result[U] = value
}

sealed trait Err extends Result[Nothing] {
  type ThisType <: Err

  def isSuccess: Boolean = false

  def map[TT](fn: Nothing => TT): Result[TT] = this.asInstanceOf[Result[TT]]

  def flatMap[TT](fn: Nothing => Result[TT]): Result[TT] = this.asInstanceOf[Result[TT]]

  def getOrElse[U >: Nothing](default: => U): U = default

  def foreach[U](f: Nothing => U): Unit = ()

  def collect[U](pf: PartialFunction[Nothing, U]): Result[U] = this

  def filter(p: Nothing => Boolean): Result[Nothing] = this

  def toOption: Option[Nothing] = None

  def toEither: Either[Err, Nothing] = Left(this)

  def fold[U](fa: Err => U, fb: Nothing => U): U = fa(this)

  def transform[U](s: Nothing => Result[U], f: Err => Result[U]): Result[U] = f(this)

  def recover[U >: Nothing](pf: PartialFunction[Err, U]): Result[U] = {
    if (pf.isDefinedAt(this)) Result.eval(pf(this)) else Err.Msg("not defined")
  }

  def recoverWith[U >: Nothing](pf: PartialFunction[Err, Result[U]]): Result[U] = {
    if (pf.isDefinedAt(this)) pf(this) else Err.Msg("not defined")
  }

  def flatten[U](implicit ev: Nothing <:< Result[U]): Result[U] = this.asInstanceOf[Result[U]]

  def nested: List[Err]

  protected def updated(children: List[Err]): ThisType

  def :+(child: Err): ThisType = updated(children = nested :+ child)

  def :+(child: String): ThisType = this :+ Err.Msg(child.trim)

  def :+(child: Throwable): ThisType = this :+ Err.Exception(child)

  def +:(child: Err): ThisType = updated(children = child +: nested)

  def +:(child: String): ThisType = Err.Msg(child.trim) +: this

  def +:(child: Throwable): ThisType = Err.Exception(child) +: this

  protected def writeValue(w: PrintWriter, indent: Int): Unit

  def write(w: PrintWriter, indent: Int = 0): Unit = {
    writeValue(w, indent)
    if (nested.nonEmpty) w.println()
    nested.zipWithIndex foreach { case (c, i) =>
      c.write(w, indent + 1)
      if (i < nested.length - 1) w.println()
    }
  }

  def merge(that: Err): Err = {
    import Err._

    (this, that) match {
      case (l: ErrList, r: ErrList)   => l.copy(nested = l.nested ++ r.nested)
      case (l: ErrList, r: Msg)       => r.copy(nested = l.nested ++ r.nested)
      case (l: ErrList, r: Exception) => r.copy(nested = l.nested ++ r.nested)

      case (l: Msg, r: ErrList)   => l.copy(nested = l.nested ++ r.nested)
      case (l: Msg, r: Exception) => ErrList(List(l, r))
      case (l: Msg, r: Msg)       => ErrList(List(l, r))

      case (l: Exception, r: ErrList)   => l.copy(nested = l.nested ++ r.nested)
      case (l: Exception, r: Exception) => ErrList(List(l, r))
      case (l: Exception, r: Msg)       => ErrList(List(l, r))
    }
  }

  def message: String = {
    val w = new StringWriter
    val pw = new PrintWriter(w)
    write(pw)
    pw.close()
    w.toString
  }
}

object Err {

  final case object NotDefined extends java.lang.Exception with NoStackTrace

  final case class ErrList(nested: List[Err] = Nil) extends Err {
    type ThisType = ErrList

    override protected def updated(children: List[Err]): ThisType = copy(nested = children)

    protected def writeValue(w: PrintWriter, indent: Int): Unit = ()
  }

  final case class Msg(
    value: String,
    nested: List[Err] = Nil) extends Err {
    type ThisType = Msg
    override protected def updated(children: List[Err]): ThisType = copy(nested = children)

    override protected def writeValue(w: PrintWriter, indent: Int): Unit = {
      val lines = value.split('\n')
      val prefix = if (indent > 0) "- " else ""
      lines.zipWithIndex foreach { case (line, i) =>
        writeIndent(w, indent)
        w.print(prefix)
        w.print(line)
        if (i < lines.length - 1) w.println()
      }
    }
  }

  final case class Exception(
    value: Throwable,
    nested: List[Err] = Nil) extends Err {
    import Exception._

    type ThisType = Exception
    override protected def updated(children: List[Err]): ThisType = copy(nested = children)

    override protected def writeValue(w: PrintWriter, indent: Int): Unit = {
      value.printStackTrace(new IndentedPrintWriter(w, indent))
    }
  }

  final object Exception {

    class IndentedPrintWriter(w: PrintWriter, indent: Int) extends PrintWriter(w) {

      override def print(x: String): Unit = {
        writeIndent(w, indent)
        w.print(x)
      }
    }
  }

  private def writeIndent(w: PrintWriter, indent: Int): Unit =
    (0 until indent * 2) foreach { _ => w.print(' ') }

  def apply(x: String, xs: Err*): Err = Msg(x, xs.toList)

  def apply(x: Throwable, xs: Err*): Err = Exception(x, xs.toList)

  // conversion should be enabled explicitly if needed
  object Conversions {

    implicit def stringToMessage(x: String): Msg = Msg(x)

    implicit def throwableToStack(x: Throwable): Exception = Exception(x)
  }

  // converters are enabled by default
  implicit class HErrorConvertersStringOps(private val x: String) extends AnyVal {
    def asErr: Msg = Msg(x)
  }

  implicit class HErrorConvertersThrowableOps(private val x: Throwable) extends AnyVal {
    def asErr: Exception = Exception(x)
  }
}

object Result {

  def eval[T](f: => T): Result[T] = try Ok(f) catch { case NonFatal(err) => Err.Exception(err) }
}