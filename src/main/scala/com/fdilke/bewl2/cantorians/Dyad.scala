package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.Dyad.μ

import scala.annotation.tailrec
import scala.language.postfixOps

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
  ): (Seq[T], Int) =
    canonicalSub(dyad, dyad.length)

  @tailrec private def canonicalSub[T](
    dyad: Seq[T],
    len: Int
  ): (Seq[T], Int) =
    if (len == 1)
      (dyad, 1)
    else {
      val len_2 = len/2
      val front = dyad.take(len_2)
      val rear = dyad.slice(len_2, len)
      if (front == rear)
        canonicalSub(front, len_2)
      else
        (dyad, len)
    }

  def apply[T](cycle: T*) =
    if (!isPowerOf2(cycle.length))
      throw new IllegalArgumentException(
        s"Cycle length ${cycle.length} is not a power of 2")
    else {
      val (canonicalCycle, length): (Seq[T], Int) =
        canonical(cycle :_*)
      new Dyad(
        canonicalCycle,
        length
      )
    }

  def η[T](t: T): Dyad[T] =
    Dyad(t)

  def μ[T](
    dd: Dyad[
      Dyad[T]
    ]
  ): Dyad[T] =
    Dyad(
      0 until Math.max(
        dd.length,
        dd.cycle map {
          _.length
        } max
      ) map { index =>
        dd(index)(index)
      } :_*
    )
}

class Dyad[T] private(
  val cycle: Seq[T],
  val length: Int
) {

  override def hashCode(): Int =
    cycle.hashCode()

  override def equals(obj: Any): Boolean =
    obj.asInstanceOf[Dyad[T]].cycle == cycle

  def map[U](f: T => U): Dyad[U] =
    Dyad(cycle map f :_*)

  def apply(index: Int): T =
    cycle(
      if (index >= 0)
        index % length
      else
        length + (index % length)
    )

  def flatMap[U](
    f: T => Dyad[U]
  ): Dyad[U] =
    μ[U](map(f))

  override def toString: String =
    "Dyad(" + cycle.mkString(",") + ")"
}
