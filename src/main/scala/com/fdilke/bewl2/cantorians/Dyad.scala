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

  @inline def canonical[T](
    dyad: T*
  ): Seq[T] =
    canonicalSub(dyad, dyad.length)

  @tailrec private def canonicalSub[T](
    dyad: Seq[T],
    len: Int
  ): Seq[T] =
    if (len == 1)
      dyad
    else {
      val len_2 = len/2
      val front = dyad.take(len_2)
      val rear = dyad.slice(len_2, len)
      if (front == rear)
        canonicalSub(front, len_2)
      else
        dyad
    }

  def apply[T](cycle: T*) =
    if (!isPowerOf2(cycle.length))
      throw new IllegalArgumentException
    else
      new Dyad(cycle)
}

class Dyad[T] private(cycle: Seq[T]) {

}
