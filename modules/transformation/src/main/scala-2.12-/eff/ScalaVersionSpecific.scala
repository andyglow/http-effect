package eff

class ScalaVersionSpecific {

  implicit class EffRightBiasedEitherOps[A, B](private val x: Either[A, B]) {

    @inline def map[BB](f: B => BB): Either[A, BB] = x.right map f

    @inline def flatMap[BB](f: B => Either[A, BB]): Either[A, BB] = x.right flatMap f

    @inline def getOrElse[BB >: B](f: => BB): BB = x.right getOrElse  f

    @inline def foreach(f: B => Unit): Unit = x.right foreach f

    @inline def toOption: Option[B] = x.right.toOption

    @inline def exists(pred: B => Boolean): Boolean = x.right.exists(pred)

    @inline def forall(pred: B => Boolean): Boolean = x.right.forall(pred)

    @inline def filter(pred: B => Boolean): Option[Either[A, B]] = x.right.filter(pred)
  }
}