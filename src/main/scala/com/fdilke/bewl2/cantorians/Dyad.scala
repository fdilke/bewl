package com.fdilke.bewl2.cantorians

import com.fdilke.bewl2.cantorians.Catcher.applyCatcher
import com.fdilke.bewl2.cantorians.Dyad.μ
import com.fdilke.bewl2.topology.Hausdorff

import scala.Function.tupled
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

  @inline def canonical[
    H: Hausdorff
  ](
    dyad: H*
  ): (Seq[H], Int) =
    canonicalSub(dyad, dyad.length)

  @tailrec private def canonicalSub[
    H: Hausdorff
  ](
    dyad: Seq[H],
    len: Int
  ): (Seq[H], Int) =
    if (len == 1)
      (dyad, 1)
    else {
      val len_2 = len / 2
      val front = dyad.take(len_2)
      val rear = dyad.slice(len_2, len)
      if (front.zip(rear).forall {
            tupled(Hausdorff[H].equalH)
          })
        canonicalSub(front, len_2)
      else
        (dyad, len)
    }

  def apply[
    H: Hausdorff
  ](
    cycle: H*
  ): Dyad[H] =
    if (!isPowerOf2(cycle.length))
      throw new IllegalArgumentException(s"Cycle length ${cycle.length} is not a power of 2")
    else {
      val (canonicalCycle, length): (Seq[H], Int) =
        canonical(cycle: _*)
      new Dyad(
        canonicalCycle,
        length
      )
    }

  def η[H: Hausdorff](t: H): Dyad[H] =
    Dyad(t)

  def μ[H: Hausdorff](
    dd: Dyad[
      Dyad[H]
    ]
  ): Dyad[H] =
    Dyad(
      (0 until Math.max(
        dd.length,
        dd.cycle.map {
          _.length
        } max
      )).map(index => dd(index)(index)): _*
    )

  implicit def jonssonTarski[
    H: Hausdorff
  ]: JonssonTarski[Dyad[H]] =
    new JonssonTarski[Dyad[H]] {
      override def join(
        l: Dyad[H],
        r: Dyad[H]
      ): Dyad[H] =
        Dyad.join(l, r)

      override def left(
        dyad: Dyad[H]
      ): Dyad[H] =
        dyad.left

      override def right(
        dyad: Dyad[H]
      ): Dyad[H] =
        dyad.right
    }

  implicit def hausdorffDyad[
    H: Hausdorff
  ]: Hausdorff[Dyad[H]] =
    Catcher.hausdorff[Dyad[H], Boolean, H]

  implicit def catcherDyad[
    H: Hausdorff
  ]: Catcher[Dyad[H], Boolean, H] =
    new Catcher[Dyad[H], Boolean, H] {
      override def either(
        dyad: Dyad[H]
      ): Either[H, Boolean => Dyad[H]] =
        if (dyad.length == 1)
          Left(dyad(0))
        else
          Right {
            if (_)
              dyad.right
            else
              dyad.left
          }

      override def construct(
        e: => Either[
          H,
          Boolean => Dyad[H]
        ]
      ): Dyad[H] = e match {
        case Left(h) => Dyad(h)
        case Right(bool2dyad) =>
          Dyad.join(
            bool2dyad(false),
            bool2dyad(true)
          )
      }
    }

  def join[H: Hausdorff](
    l: Dyad[H],
    r: Dyad[H]
  ): Dyad[H] =
    Dyad(
      Seq.concat(
        (0 until Math.max(l.length, r.length)).map { index =>
          Seq(
            l(index),
            r(index)
          )
        }: _*
      ): _*
    )
}

class Dyad[H: Hausdorff] private (
  val cycle: Seq[H],
  val length: Int
) {
  override def hashCode(): Int =
    Hausdorff.intKey(this)

  override def equals(obj: Any): Boolean =
    Hausdorff.equalH(
      obj.asInstanceOf[Dyad[H]],
      this
    )

  def map[
    K: Hausdorff
  ](
    f: H => K
  ): Dyad[K] =
    Dyad(
      cycle.map(f): _*
    )

  def apply(index: Int): H =
    cycle(
      if (index >= 0)
        index % length
      else
        length + (index % length)
    )

  def apply[P](p: P)(
    implicit pitcherTude: Pitcher[P, Boolean]
  ): H =
    applyCatcher(this)(p)

  def flatMap[
    K: Hausdorff
  ](
    f: H => Dyad[K]
  ): Dyad[K] =
    μ[K](map(f))

  override def toString: String =
    "Dyad(" + cycle.mkString(",") + ")"

  def left: Dyad[H] =
    Dyad(
      Range(0, cycle.length, 2).map {
        apply
      }: _*
    )

  def right: Dyad[H] =
    Dyad(
      Range(1, cycle.length + 1, 2).map {
        apply
      }: _*
    )
}
