package com.fdilke.bewl2.actions

import com.fdilke.bewl2.algebra.AlgebraicMachinery
import com.fdilke.bewl2.ProductMappable
import com.fdilke.bewl2.{ PreTopos, Topos }

trait MonoidActions[
  DOT[_],
  CTXT[_]: ProductMappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
] extends AlgebraicMachinery[DOT, CTXT, VOID, UNIT, BEWL, >]:
  topos: Topos[DOT, CTXT, VOID, UNIT, BEWL, >] =>

  class MonoidActions[M: Dot](
    monoid: Monoid[M]
  ) extends Topos[
    Set, [A] =>> A, Void, Unit, Boolean, Map
  ](PreMonoidActions(monoid)):
    val pig = 1

  class PreMonoidActions[M: Dot](
    monoid: Monoid[M]
  ) extends PreTopos[Set, [A] =>> A, Void, Unit, Boolean, Map]:
    override val unitDot: Set[Unit] =
      ???

    override val zeroDot: Set[Void] =
      ???

    override val omegaDot: Set[Boolean] =
      ???

    override val truth: Unit => Boolean =
      ???

    override def equalArrows[X, Y](
      dotX: Set[X],
      dotY: Set[Y],
      f1: X => Y,
      f2: X => Y
    ): Boolean =
      ???

    override def uncachedProductObject[X, Y](
      dotX: Set[X],
      dotY: Set[Y]
    ): Set[(X, Y)] =
      ???

    override def uncachedExponentialObject[X, Y](
      dotX: Set[X],
      dotY: Set[Y]
    ): Set[X Map Y] =
      ???
    
    override def sanityTest[X](
      dotX: Set[X]
    ): Unit =
      ???

    override def sanityTest[X, Y](
      dotX: Set[X],
      dotY: Set[Y],
      f: X ~> Y
    ): Unit =
      ???

    override def enumerateMorphisms[X, Y](
      dotX: Set[X],
      dotY: Set[Y]
    ): Iterable[X => Y] =
      ???

    override def fromZero[X](dotX: Set[X]): Void => X =
      ???

    override def toUnit[X](
      dotX: Set[X]
    ): X => Unit =
      ???

    override def evaluation[X, Y](
      dotX: Set[X],
      dotY: Set[Y]
    ): ((X Map Y, X)) => Y =
      ???

    override def transpose[X, Y, Z](
      dotX: Set[X],
      dotY: Set[Y],
      dotZ: Set[Z],
      xy2z: ((X, Y)) => Z
    ): X => (Y Map Z) =
      ???

    override def doEqualizer[X, Y, RESULT](
      dotX: Set[X],
      dotY: Set[Y],
      f: X => Y,
      f2: X => Y
    )(
      block: [A] => RawEqualizer[A, X] => Set[A] => RESULT
    ): RESULT =
      ???

    override def chiForMonic[X, Y](
      dotX: Set[X],
      dotY: Set[Y],
      monic: X => Y
    ): Y => Boolean =
      ???

    override def backDivideMonic[X, Y, A](
      dotX: Set[X],
      dotY: Set[Y],
      dotA: Set[A],
      arrow: X => Y,
      monic: A => Y
    ): X => A =
      ???









