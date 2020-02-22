package com.fdilke.bewl2.util

class FunctionWithEquality[S, T](domain: Iterable[S], val innerFunction: S => T) extends Function[S, T] {
  override def apply(s: S): T =
    innerFunction(s)

  // logically necessary: so don't use these wrapped functions as the keys of hash
  override def hashCode(): Int = 0

  override def equals(other: Any): Boolean =
    try {
        val that = other.asInstanceOf[FunctionWithEquality[S, T]]
        domain.forall { s =>
          innerFunction(s) == that.innerFunction(s)
        }
    } catch { case _: ClassCastException =>
      false
    }
}
