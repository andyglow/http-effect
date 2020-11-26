package eff.internal

object VersionFix {

  @inline def map[L, R, RR](x: Either[L, R], fn: R => RR): Either[L, RR] = x map fn

  @inline def flatMap[L, R, RR](x: Either[L, R], fn: R => Either[L, RR]): Either[L, RR] = x flatMap fn
}
