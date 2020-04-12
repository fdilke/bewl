package com.fdilke.bewl2.fsets

import com.fdilke.bewl2.fsets.FiniteSets.FiniteSetsTopos
import com.fdilke.bewl2.fsets.FiniteSetsUtilities.allMaps
import com.fdilke.bewl2.topos.FunctionalPlumbing.{CharacteristicArrow, Equalizer, EqualizerReceiver}
import com.fdilke.bewl2.topos.Topos
import com.fdilke.bewl2.util.FunctionWithEquality

import scala.language.postfixOps

object FiniteSets {
  implicit object FiniteSetsTopos extends Topos[Iterable] {
    override def sanityTest[S: Iterable]: Unit =
      println(s"Sanity testing ${dot[S]}")

    override def sanityTest[S: Iterable, T: Iterable](arrow: S => T): Unit =
      for { s <- dot[S]; t = arrow(s) } if (!dot[T].exists(_ == t))
        throw new IllegalArgumentException(
          "Broken arrow: " + functionAsString(arrow) +
            s" maps value $s outside domain ${dot[S]}"
        )

    override def functionAsString[S: Iterable, T: Iterable](
      arrow: S => T
    ): String =
      (dot[S].map(s => s -> arrow(s)) toMap) toString

    override def compareFunctions[S: Iterable, T: Iterable](
      func: S => T,
      func2: S => T
    ): Boolean =
      dot[S].forall(s => func(s) == func2(s))

    override def equalize[
      S: Iterable,
      T: Iterable,
      X
    ](
      func1: S => T,
      func2: S => T
    ): EqualizerReceiver[Iterable, S, X] => X = {
      @inline type R = S
      val subset: Iterable[R] = dot[S].filter(s => func1(s) == func2(s))
      _(new Equalizer[Iterable, S, R] {
        override val include: R => S =
          identity
        override def restrict[Q: Iterable](
          arrow: Q => S
        ): Q => R =
          arrow
      })(subset)
    }

    override def productUncached[
      A: Iterable,
      B: Iterable
    ]: Iterable[(A, B)] =
      for {
        a <- dot[A]
        b <- dot[B]
      } yield {
        a -> b
      }

    implicit override val terminator: Iterable[Unit] =
      Iterable {}

    override def to1[S: Iterable]: S => Unit =
      _ => {}

    implicit override val initial: Iterable[Void] =
      Iterable[Void]()

    override def from0[S: Iterable]: Void => S =
      _ => throw new IllegalArgumentException("You passed a Void")

    override type >[A, B] = FunctionWithEquality[Iterable, A, B]

    override def exponentialUncached[
      A: Iterable,
      B: Iterable
    ]: Iterable[A > B] =
      allMaps(dot[A], dot[B]).map(f => new FunctionWithEquality[Iterable, A, B](f))

    override def transpose[
      A: Iterable,
      B: Iterable,
      C: Iterable
    ](
      arrow: (A, B) => C
    ): A => B > C =
      a =>
        new FunctionWithEquality[Iterable, B, C](
          arrow(a, _)
        )

    override type Ω = Boolean
    implicit override val omega: Iterable[Boolean] =
      Iterable(true, false)
    override val truth: Unit => Boolean =
      _ => true

    override def chi[S: Iterable, T: Iterable](
      monic: S => T
    ): CharacteristicArrow[Iterable, S, T, Boolean] =
      new CharacteristicArrow[Iterable, S, T, Boolean] {
        override val chi: T => Boolean =
          t =>
            dot[S].exists {
              monic(_) == t
            }

        override def restrict[R: Iterable](
          arrow: R => T
        ): R => S =
          r => {
            val target = arrow(r)
            dot[S].find {
              monic(_) == target
            }.get
          }
      }

  }
}
