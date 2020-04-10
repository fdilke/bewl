package com.fdilke.bewl2.util

import com.fdilke.bewl2.topos.Topos

class FunctionWithEquality[
  DOT[_]: Topos,
  S: DOT,
  T: DOT
](
  val innerFunction: S => T
) extends Function[S, T] {

  override def apply(s: S): T =
    innerFunction(s)

  private val topos: Topos[DOT] = Topos[DOT]
  import topos._

  // logically necessary: so don't use these wrapped functions as the keys of hash
  override def hashCode(): Int = 0

  override def equals(other: Any): Boolean =
    try {
      val that = other.asInstanceOf[FunctionWithEquality[DOT, S, T]]
      innerFunction =?= that.innerFunction
    } catch {
      case _: ClassCastException =>
        false
    }
}
