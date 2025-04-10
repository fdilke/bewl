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
  ):
    val theDot: Dot[A] =
      summon

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
        given Ɛ.Dot[X] = dotX.theDot
        given Ɛ.Dot[Y] = dotY.theDot
        f1 =!= f2

      override def uncachedProductObject[X, Y](
        dotX: Automorphism[X],
        dotY: Automorphism[Y]
      ): Automorphism[(X, Y)] =
        given Ɛ.Dot[X] = dotX.theDot
        given Ɛ.Dot[Y] = dotY.theDot
        Automorphism[(X, Y)](
          (dotX.arrow o Ɛ.π0[X, Y]) x (dotY.arrow o Ɛ.π1[X, Y]),
          (dotX.inverse o Ɛ.π0[X, Y]) x (dotY.inverse o Ɛ.π1[X, Y])
        )

      override def uncachedExponentialObject[X, Y](
        dotX: Automorphism[X],
        toolkitX: Unit,
        dotY: Automorphism[Y],
        toolkitY: Unit
      ): Automorphism[X > Y] =
        given Ɛ.Dot[X] = dotX.theDot
        given Ɛ.Dot[Y] = dotY.theDot
        def twist(
          arrowX: X ~> X,
          arrowY: Y ~> Y
        ): (X > Y) ~> (X > Y) =
          Ɛ.transpose[X > Y, X, Y]:
            case Ɛ.⊕(f, x) =>
              arrowY:
                Ɛ.evaluation[X, Y].apply:
                  f ⊕ arrowX(x)
        new Automorphism[X > Y](
          twist(dotX.inverse, dotY.arrow),
          twist(dotX.arrow, dotY.inverse)
        )

      override def sanityTest[X](
        dotX: Automorphism[X]
      ): Unit =
        given Ɛ.Dot[X] = dotX.theDot
        assert:
          (dotX.arrow o dotX.inverse) =!= Ɛ.id[X]
        assert:
          (dotX.inverse o dotX.arrow) =!= Ɛ.id[X]

      override def sanityTest[X, Y](
        dotX: Automorphism[X],
        dotY: Automorphism[Y],
        f: X ~> Y
      ): Unit =
        given Ɛ.Dot[X] = dotX.theDot
        given Ɛ.Dot[Y] = dotY.theDot
        assert:
          (dotY.arrow o f) =!= (f o dotX.arrow)

      private def liftIdentity[X: Ɛ.Dot]: Automorphism[X] =
        val identityX: X ~> X =
          Ɛ.id[X]
        new Automorphism[X](
          identityX,
          identityX
        )

      override val unitDot: Automorphism[UNIT] =
        liftIdentity[UNIT]

      override val zeroDot: Automorphism[VOID] =
        liftIdentity[VOID]

      override val omegaDot: Automorphism[BEWL] =
        liftIdentity[BEWL]

      override def toUnit[X](
        dotX: Automorphism[X]
      ): X ~> UNIT =
        given Ɛ.Dot[X] = dotX.theDot
        Ɛ.toUnit[X]

      override def fromZero[X](
        dotX: Automorphism[X]
      ): VOID ~> X =
        given Ɛ.Dot[X] = dotX.theDot
        Ɛ.fromZero[X]

      override val truth: UNIT ~> BEWL =
        Ɛ.truth

      override def enumerateMorphisms[X, Y](
        dotX: Automorphism[X],
        toolkitX: Unit,
        dotY: Automorphism[Y],
        toolkitY: Unit
      ): Iterable[X ~> Y] =
        given Ɛ.Dot[X] = dotX.theDot
        given Ɛ.Dot[Y] = dotY.theDot
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
