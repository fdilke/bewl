package com.fdilke.bewl2.topology

import com.fdilke.bewl2.topology.Compact._
import com.fdilke.bewl2.util.FindSingleton

import scala.language.{implicitConversions, postfixOps}
import scala.reflect.runtime.universe._

trait Compact[T] {
  def find(
    predicate: T => Boolean
  ): Option[
    () => T
  ]

  lazy val optional: Option[T] =
    find { _ => true } map {
      _()
    }
}

object Compact {

  implicit def CompactnessForEnum[
    ENUM <: Enumeration : TypeTag
  ]: Compact[ENUM#Value] =
    (predicate: ENUM#Value => Boolean) =>
      FindSingleton[ENUM].values find predicate map {
        v => () => v
      }

  @inline def find[T : Compact](
    predicate: T => Boolean
  ): Option[
    () => T
  ] =
    implicitly[Compact[T]] find(
      predicate
    )

  @inline def exists[T : Compact](
    predicate: T => Boolean
  ): Boolean =
    find[T](
      predicate
    ) isDefined

  @inline def forAll[T : Compact](
    predicate: T => Boolean
  ): Boolean =
    ! exists[T] {
      !predicate(_)
    }

  @inline def inhabited[T : Compact]: Boolean =
    exists[T] { _ => true }
}

trait Hausdorff[T] {
  def equalH(
    t1: T,
    t2: T
  ): Boolean

  def intKey(t: T): Int
}

object Hausdorff {

  def standardHausdorff[T]: Hausdorff[T] =
    new Hausdorff[T] {
      override def equalH(t1: T, t2: T): Boolean =
        t1 == t2

      override def intKey(t: T): Int =
        t.hashCode
    }

  implicit def HausdorffnessForEnum[
    ENUM <: Enumeration
  ]: Hausdorff[ENUM#Value] =
    standardHausdorff[ENUM#Value]

  implicit def HausdorffnessForInt[
    Int
  ]: Hausdorff[Int] =
    standardHausdorff[Int]

  class Key[H](
    h: H
  )(
    implicit hausdorff: Hausdorff[H]
  ) {
    override def hashCode(): Int =
      hausdorff.intKey(h)

    override def equals(h2: Any): Boolean =
      hausdorff.equalH(h, h2.asInstanceOf[H])

    override def toString: String =
      h.toString
  }

  @inline def equalH[T : Hausdorff](
    t1: T,
    t2: T
  ): Boolean =
    implicitly[Hausdorff[T]] equalH(
      t1, t2
    )

  implicit def hausdorffExponential[
    C : Compact,
    H : Hausdorff
  ]: Hausdorff[
    C => H
  ] = new Hausdorff[C => H] {
    override def equalH(
      f: C => H,
      g: C => H
    ): Boolean =
      forAll[C] { c =>
        implicitly[Hausdorff[H]].equalH(
          f(c),
          g(c)
        )
      }

    override def intKey(f: C => H): Int =
      implicitly[Compact[C]].optional match {
        case None => 0
        case Some(c) =>
          implicitly[Hausdorff[H]].intKey(
            f(c)
          )
      }
  }
}
