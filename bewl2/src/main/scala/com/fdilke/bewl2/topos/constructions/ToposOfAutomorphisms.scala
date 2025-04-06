package com.fdilke.bewl2.topos.constructions

import com.fdilke.bewl2.algebra.AlgebraicMachinery
import com.fdilke.bewl2.topos.{PreToposWithDefaultToolkit, ProductMappable, Topos}
import ProductMappable.*

trait ToposOfAutomorphisms[
  DOT[_],
  CTXT[_]: ProductMappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
]:
  Ɛ: Topos[DOT, CTXT, VOID, UNIT, BEWL, >] =>

  case class Automorphism[A : Dot](
    arrow: A ~> A,
    inverse: A ~> A
  )

  object Automorphism:
    def apply[A : Dot](
      arrow: A ~> A
    ): Automorphism[A] =
      Automorphism[A](
        arrow,
        arrow.inverse
      )

  lazy val toposOfAutomorphisms: Topos[
    Automorphism,
    CTXT,
    VOID, 
    UNIT, 
    BEWL,
    >
  ] =
    new PreToposWithDefaultToolkit[Automorphism, CTXT, VOID, UNIT, BEWL, >]:

      override def equalArrows[X, Y](
        dotX: Automorphism[X],
        dotY: Automorphism[Y],
        f1: X ~> Y,
        f2: X ~> Y
      ): Boolean =
        ???

      override def uncachedProductObject[X, Y](
        dotX: Automorphism[X],
        dotY: Automorphism[Y]
      ): Automorphism[(X, Y)] =
        ???

      override def uncachedExponentialObject[X, Y](
        dotX: Automorphism[X],
        toolkitX: Unit,
        dotY: Automorphism[Y],
        toolkitY: Unit
      ): Automorphism[X > Y] =
        ???

      override def sanityTest[X](
        dotX: Automorphism[X]
      ): Unit =
        ???

      override def sanityTest[X, Y](
        dotX: Automorphism[X],
        dotY: Automorphism[Y],
        f: X ~> Y
      ): Unit =
        ???

      override val unitDot: Automorphism[UNIT] =
        ???

      override val zeroDot: Automorphism[VOID] =
        ???

      override val omegaDot: Automorphism[BEWL] =
        ???

      override def toUnit[X](
        dotX: Automorphism[X]
      ): X ~> UNIT =
        ???

      override def fromZero[X](
        dotX: Automorphism[X]
      ): VOID ~> X = ???

      override val truth: UNIT ~> BEWL =
        ???

      override def enumerateMorphisms[X, Y](
        dotX: Automorphism[X],
        toolkitX: Unit,
        dotY: Automorphism[Y],
        toolkitY: Unit
      ): Iterable[X ~> Y] =
        ???

      override def evaluation[X, Y](
        dotX: Automorphism[X],
        dotY: Automorphism[Y]
      ): (X > Y, X) ~> Y =
        ???

      override def transpose[X, Y, Z](
        dotX: Automorphism[X],
        dotY: Automorphism[Y],
        dotZ: Automorphism[Z],
        xy2z: (X, Y) ~> Z
      ): X ~> (Y > Z) =
        ???

      override def doEqualizer[X, Y, RESULT](
        dotX: Automorphism[X],
        dotY: Automorphism[Y],
        f: X ~> Y,
        f2: X ~> Y
      )(
        capture: [A] => RawEqualizer[A, X] => Automorphism[A] => RESULT
      ): RESULT =
        ???

      override def chiForMonic[X, Y](
        dotX: Automorphism[X],
        dotY: Automorphism[Y],
        monic: X ~> Y
      ): Y ~> BEWL = ???

      override def backDivideMonic[X, Y, A](
        dotX: Automorphism[X],
        dotY: Automorphism[Y],
        dotA: Automorphism[A],
        arrow: X ~> Y,
        monic: A ~> Y
      ): X ~> A =
        ???

    .toTopos


