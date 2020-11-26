package eff


trait Throw[T] {

  def apply(x: T): Nothing
}

object Throw {

  def apply[T](fn: T => Throwable): Throw[T] = new Throw[T] { def apply(x: T): Nothing = throw fn(x) }

  implicit val throwThr: Throw[Throwable] = Throw { identity }
  implicit val throwStr: Throw[String]    = Throw { ThrowableErr.apply }
  implicit val throwErr: Throw[Err]       = Throw {
    case Err.Exception(x, Nil)  => x
    case x                      => ThrowableErr(x)
  }
}
