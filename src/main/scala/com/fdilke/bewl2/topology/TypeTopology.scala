package com.fdilke.bewl2.topology

import java.util.concurrent.atomic.AtomicReference

import com.fdilke.bewl2.cantorians.{Cantorian, Pitcher, VanillaPitcher}
import com.fdilke.bewl2.topology.Compact._
import com.fdilke.bewl2.topology.HausdorffToCompactPredicateSolver.{functionFromMap, solveMap}
import com.fdilke.bewl2.util.FindSingleton

import scala.language.{implicitConversions, postfixOps}
import scala.reflect.runtime.universe._
import Pitcher._

trait Compact[T] {
  def find(
    predicate: T => Boolean
  ): Option[
    () => T
  ]
}

object Compact {

  def apply[T](
    implicit compact: Compact[T]
  ): Compact[T] = compact

  implicit def CompactnessForEnum[
    ENUM <: Enumeration: TypeTag
  ]: Compact[ENUM#Value] =
    (predicate: ENUM#Value => Boolean) =>
      FindSingleton[ENUM].values.find(predicate).map(v => () => v)

  @inline def find[T: Compact](
    predicate: T => Boolean
  ): Option[
    () => T
  ] =
    Compact[T].find(predicate)

  def determine[
    T: Compact,
    U
  ](
    f: T => U,
    predicate: U => Boolean
  ): Option[(T, U)] = {
    val holder: AtomicReference[(T, U)] =
      new AtomicReference[(T, U)]()
    if (exists[T] { t =>
          val u: U = f(t)
          val satisfied: Boolean =
            predicate(u)
          if (satisfied)
            holder.set(t -> u)
          satisfied
        })
      Some(holder.get())
    else
      None
  }

  @inline def determine[T: Compact](
    predicate: T => Boolean
  ): Option[T] =
    find[T](
      predicate
    ).map {
      _()
    }

  @inline def optional[T: Compact]: Option[T] =
    determine[T](_ => true)

  @inline def exists[T: Compact](
    predicate: T => Boolean
  ): Boolean =
    find[T](
      predicate
    ) isDefined

  @inline def forAll[T: Compact](
    predicate: T => Boolean
  ): Boolean =
    !exists[T] {
      !predicate(_)
    }

  @inline def inhabited[T: Compact]: Boolean =
    optional[T].isDefined

  implicit val compactBoolean: Compact[Boolean] =
    predicate =>
      if (predicate(true))
        Some(() => true)
      else if (predicate(false))
        Some(() => false)
      else
        None

  implicit def compactExponential[
    H: Hausdorff,
    C: Compact
  ]: Compact[
    H => C
  ] =
    predicate =>
      solveMap(
        predicate
      ).map(map => () => functionFromMap(map))
}

trait Hausdorff[T] {
  def equalH(
    t1: T,
    t2: T
  ): Boolean

  def intKey(t: T): Int
}

object Hausdorff {
  def via[
    H,
    K: Hausdorff
  ](
    fn: H => K
  ): Hausdorff[H] =
    new Hausdorff[H] {
      override def equalH(h1: H, h2: H): Boolean =
        Hausdorff.equalH(
          fn(h1),
          fn(h2)
        )

      override def intKey(h: H): Int =
        Hausdorff[K].intKey(
          fn(h)
        )
    }

  def apply[T](
    implicit hausdorff: Hausdorff[T]
  ): Hausdorff[T] = hausdorff

  def standardHausdorff[T]: Hausdorff[T] =
    new Hausdorff[T] {
      override def equalH(t1: T, t2: T): Boolean =
        t1 == t2

      override def intKey(t: T): Int =
        t.hashCode
    }

  implicit def hausdorffnessForEnum[
    ENUM <: Enumeration
  ]: Hausdorff[ENUM#Value] =
    standardHausdorff[ENUM#Value]

  implicit def hausdorffnessForSeq[
    H: Hausdorff
  ]: Hausdorff[Seq[H]] =
    new Hausdorff[Seq[H]] {
      override def equalH(
        seq1: Seq[H],
        seq2: Seq[H]
      ): Boolean =
        seq1.length == seq2.length &&
          seq1.zip(seq2).forall {
            case (s, t) =>
              Hausdorff.equalH(s, t)
          }

      override def intKey(seq: Seq[H]): Int =
        seq.map(Hausdorff[H].intKey) sum
    }

  implicit val hausdorffnessForInt: Hausdorff[Int] =
    standardHausdorff[Int]

  implicit val hausdorffnessForString: Hausdorff[String] =
    standardHausdorff[String]

  implicit val hausdorffnessForBoolean: Hausdorff[Boolean] =
    standardHausdorff[Boolean]

  implicit val hausdorffnessForDouble: Hausdorff[Double] =
    standardHausdorff[Double]

  class Key[H](
    val h: H
  )(
    implicit hausdorff: Hausdorff[H]
  ) {
    override def hashCode(): Int =
      hausdorff.intKey(h)

    override def equals(h2: Any): Boolean =
      hausdorff.equalH(h, h2.asInstanceOf[Key[H]].h)

    override def toString: String =
      h.toString
  }

  @inline def equalH[H: Hausdorff](
    t1: H,
    t2: H
  ): Boolean =
    Hausdorff[H].equalH(t1, t2)

  @inline def intKey[H: Hausdorff](
    t: H
  ): Int =
    Hausdorff[H].intKey(t)

  implicit def hausdorffExponential[
    C: Compact,
    H: Hausdorff
  ]: Hausdorff[
    C => H
  ] = new Hausdorff[C => H] {
    override def equalH(
      f: C => H,
      g: C => H
    ): Boolean =
      forAll[C] { c =>
        Hausdorff[H].equalH(
          f(c),
          g(c)
        )
      }

    override def intKey(f: C => H): Int =
      optional[C] match {
        case None => 0
        case Some(c) =>
          Hausdorff[H].intKey(
            f(c)
          )
      }
  }
}
