package eff


trait Merge[T] {

  def apply(a: T, b: T): T
}

object Merge {

  private def mk[T](fn: (T, T) => T): Merge[T] = new Merge[T] {
    override def apply(a: T, b: T): T = fn(a, b)
  }

  implicit val strMerge: Merge[String]                          = mk { (a, b) => s"$a\n$b" }
  implicit val thrMerge: Merge[Throwable]                       = mk { (a, b) => a.addSuppressed(b); a }
  implicit val errMerge: Merge[Err]                             = mk { _ merge _ }
  implicit def numMerge[T](implicit num: Numeric[T]): Merge[T]  = mk { num.plus }
  implicit def listMerge[T]: Merge[List[T]]                     = mk { _ ++ _ }
  implicit def setMerge[T]: Merge[Set[T]]                       = mk { _ ++ _ }
  implicit def seqMerge[T]: Merge[Seq[T]]                       = mk { _ ++ _ }
  implicit def iterMerge[T]: Merge[Iterable[T]]                 = mk { _ ++ _ }
}
