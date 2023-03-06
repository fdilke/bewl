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

  def toposOfMonoidActions[M: Dot](
    monoid: Monoid[M]
  ): Topos[monoid.Action, [A] =>> A, VOID, UNIT, monoid.RightIdeal, Map] =
    // val pretopos = new PreTopos[
    new Topos[monoid.Action, [A] =>> A, VOID, UNIT, monoid.RightIdeal, Map](      
      new PreTopos[
        monoid.Action, [A] =>> A, VOID, UNIT, monoid.RightIdeal, Map
      ]:
        override val unitDot: monoid.Action[UNIT] =
          ???

        override val zeroDot: monoid.Action[VOID] =
          ???

        override val omegaDot: monoid.Action[monoid.RightIdeal] =
          ???

        override val truth: UNIT => monoid.RightIdeal =
          ???

        override def equalArrows[X, Y](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y],
          f1: X => Y,
          f2: X => Y
        ): Boolean =
          ???

        override def uncachedProductObject[X, Y](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y]
        ): monoid.Action[(X, Y)] =
          ???

        override def uncachedExponentialObject[X, Y](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y]
        ): monoid.Action[X Map Y] =
          ???
        
        override def sanityTest[X](
          dotX: monoid.Action[X]
        ): Unit =
          ???

        override def sanityTest[X, Y](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y],
          f: X ~> Y
        ): Unit =
          ???

        override def enumerateMorphisms[X, Y](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y]
        ): Iterable[X => Y] =
          ???

        override def fromZero[X](dotX: monoid.Action[X]): VOID => X =
          ???

        override def toUnit[X](
          dotX: monoid.Action[X]
        ): X => UNIT =
          ???

        override def evaluation[X, Y](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y]
        ): ((X Map Y, X)) => Y =
          ???

        override def transpose[X, Y, Z](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y],
          dotZ: monoid.Action[Z],
          xy2z: ((X, Y)) => Z
        ): X => (Y Map Z) =
          ???

        override def doEqualizer[X, Y, RESULT](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y],
          f: X => Y,
          f2: X => Y
        )(
          block: [A] => RawEqualizer[A, X] => monoid.Action[A] => RESULT
        ): RESULT =
          ???

        override def chiForMonic[X, Y](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y],
          monic: X => Y
        ): Y => monoid.RightIdeal =
          ???

        override def backDivideMonic[X, Y, A](
          dotX: monoid.Action[X],
          dotY: monoid.Action[Y],
          dotA: monoid.Action[A],
          arrow: X => Y,
          monic: A => Y
        ): X => A =
          ???
    )

  // class MonoidActions[M: Dot](
  //   val monoid: Monoid[M]
  // )

  // class MonoidActions[M: Dot](
  //   val monoid: Monoid[M]
  // ) extends Topos[
  //   monoid.Action, [A] =>> A, VOID, UNIT, monoid.RightIdeal, Map
  // ](PreMonoidActions(monoid))

  // class PreMonoidActions[M: Dot](
  //   val monoid: Monoid[M]
  // ) extends 
  

