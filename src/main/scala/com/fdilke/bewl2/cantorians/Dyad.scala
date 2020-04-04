package com.fdilke.bewl2.cantorians

import scala.annotation.tailrec

object Dyad {
  @tailrec
  def isPowerOf2(n: Int): Boolean =
    if (n < 1)
      false
    else if (n == 1)
      true
    else if (n % 2 == 1)
      false
    else
      isPowerOf2(n / 2)


  def apply[T](cycle: T*) =
    if (!isPowerOf2(cycle.length))
      throw new IllegalArgumentException
    else
      new Dyad(cycle)
}

class Dyad[T] private(cycle: Seq[T]) {

}
