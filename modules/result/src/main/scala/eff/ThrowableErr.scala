package eff

import scala.util.control.NoStackTrace


final case class ThrowableErr(value: Err) extends Exception with NoStackTrace {

  override def getMessage: String = value.message
}

object ThrowableErr {

  def apply(msg: String): ThrowableErr = ThrowableErr(Err.Msg(msg))

  def apply(thr: Throwable): ThrowableErr = ThrowableErr(Err.Exception(thr))
}