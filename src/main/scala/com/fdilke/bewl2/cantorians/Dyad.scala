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
      throw new IllegalArgumentException(
        s"Cycle length ${cycle.length} is not a power of 2")
    else
      new Dyad(
        canonical(cycle :_*)
      )
}

class Dyad[T] private(val cycle: Seq[T]) {
  override def hashCode(): Int =
    cycle.hashCode()

  override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Dyad[T]].cycle == cycle

  def map[U](f: T => U): Dyad[U] =
    Dyad(cycle map f :_*)
}
