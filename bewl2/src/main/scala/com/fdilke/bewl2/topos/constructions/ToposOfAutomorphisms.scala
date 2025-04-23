package com.fdilke.bewl2.topos.constructions

import com.fdilke.bewl2.algebra.AlgebraicMachinery
import com.fdilke.bewl2.topos.{PreToposWithDefaultToolkit, ProductMappable, Topos}
import ProductMappable.*

import scala.language.postfixOps

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
    val theDot: Ɛ.Dot[A] =
      summon

  object Automorphism:
    def apply[A : Dot](
      arrow: A ~> A
    ): Automorphism[A] =
      Automorphism[A](
        arrow,
        arrow.inverse
      )
    def id[A: Dot]: Automorphism[A] =
      Automorphism[A](
        Ɛ.id[A],
        Ɛ.id[A]
      )

  private object preAutoTopos extends PreToposWithDefaultToolkit[Automorphism, CTXT, VOID, UNIT, BEWL, >]:
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

    private inline def isMorphism[X, Y](
      dotX: Automorphism[X],
      dotY: Automorphism[Y],
      f: X ~> Y
    ): Boolean =
      given Ɛ.Dot[X] = dotX.theDot
      given Ɛ.Dot[Y] = dotY.theDot
      (dotY.arrow o f) =!= (f o dotX.arrow)

    override def sanityTest[X, Y](
      dotX: Automorphism[X],
      dotY: Automorphism[Y],
      f: X ~> Y
    ): Unit =
      assert:
        isMorphism[X, Y](dotX, dotY, f)

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
      Ɛ.morphisms[X, Y].filter: f =>
        isMorphism[X, Y](dotX, dotY, f)

    override def evaluation[X, Y](
      dotX: Automorphism[X],
      dotY: Automorphism[Y]
    ): (X > Y, X) ~> Y =
      given Ɛ.Dot[X] = dotX.theDot
      given Ɛ.Dot[Y] = dotY.theDot
      Ɛ.evaluation[X, Y]

    override def transpose[X, Y, Z](
      dotX: Automorphism[X],
      dotY: Automorphism[Y],
      dotZ: Automorphism[Z],
      xy2z: (X, Y) ~> Z
    ): X ~> (Y > Z) =
      given Ɛ.Dot[X] = dotX.theDot
      given Ɛ.Dot[Y] = dotY.theDot
      given Ɛ.Dot[Z] = dotZ.theDot
      Ɛ.transpose[X, Y, Z]:
        xy2z

    override def doEqualizer[X, Y, RESULT](
      dotX: Automorphism[X],
      dotY: Automorphism[Y],
      f: X ~> Y,
      f2: X ~> Y
    )(
      capture: [A] => RawEqualizer[A, X] => Automorphism[A] => RESULT
    ): RESULT =
      given Ɛ.Dot[X] = dotX.theDot
      given Ɛ.Dot[Y] = dotY.theDot
      Ɛ.doEqualizer[X, Y, RESULT](f, f2):
        [A] => (dotA: Dot[A]) ?=> (equalizer: Ɛ.Equalizer[A, X]) =>
          capture[A](
            new RawEqualizer[A, X]:
              override val inclusion: A ~> X =
                equalizer.inclusion
              override def restrict[R](
                dotR: Automorphism[R],
                arrow: R ~> X
              ): R ~> A =
                given Ɛ.Dot[R] = dotR.theDot
                equalizer.restrict[R]:
                  arrow
          )(
            Automorphism[A](
              equalizer.restrict:
                dotX.arrow o equalizer.inclusion
              ,
              equalizer.restrict:
                dotX.inverse o equalizer.inclusion
            )
          )

    override def chiForMonic[X, Y](
      dotX: Automorphism[X],
      dotY: Automorphism[Y],
      monic: X ~> Y
    ): Y ~> BEWL =
      given Ɛ.Dot[X] = dotX.theDot
      given Ɛ.Dot[Y] = dotY.theDot
      Ɛ.chiForMonic[X, Y]:
        monic

    override def backDivideMonic[X, Y, A](
      dotX: Automorphism[X],
      dotY: Automorphism[Y],
      dotA: Automorphism[A],
      arrow: X ~> Y,
      monic: A ~> Y
    ): X ~> A =
      given Ɛ.Dot[X] = dotX.theDot
      given Ɛ.Dot[Y] = dotY.theDot
      given Ɛ.Dot[A] = dotA.theDot
      Ɛ.backDivideMonic[X, Y, A](
        arrow,
        monic
      )

  lazy val toposOfAutomorphisms: Topos[
    Automorphism,
    CTXT,
    VOID, 
    UNIT, 
    BEWL,
    >
  ] =
    new Topos[Ɛ.Automorphism, CTXT, VOID, UNIT, BEWL, >](
      preAutoTopos
    ):
      implicit inline def dotFromAuto[X: Dot]: Ɛ.Dot[X] =
        summon[Dot[X]].dot.theDot

      override lazy val logicalOperations: LogicalOperations =
        new LogicalOperations:
          override val and: BiArrow[BEWL, BEWL, BEWL] =
            Ɛ.logicalOperations.and
          override val implies: BiArrow[BEWL, BEWL, BEWL] =
            Ɛ.logicalOperations.implies
          override val or: BiArrow[BEWL, BEWL, BEWL] =
            Ɛ.logicalOperations.or
          override val falsity: NullaryOp[BEWL] =
            Ɛ.logicalOperations.falsity

      override lazy val monicVerifier: MonicVerifier =
        new MonicVerifier:
          override def isMonic[X: Dot, Y: Dot](
            f: X ~> Y
          ): Boolean =
            Ɛ.monicVerifier.isMonic(f)

      override lazy val epicVerifier: EpicVerifier =
        new EpicVerifier:
          override def isEpic[X: Dot, Y: Dot](
            f: X ~> Y
          ): Boolean =
            Ɛ.epicVerifier.isEpic(f)

      override lazy val autoFinder: AutomorphismFinder =
        new AutomorphismFinder:
          override def withAutomorphismGroup[X: Dot, RESULT](
            block: [A] => Dot[A] ?=>
              (group: Group[A]) ?=> group.Action[X] ?=> RESULT
          ): RESULT =
            Ɛ.autoFinder.withAutomorphismGroup[X, RESULT]:
              [A] => (_: Ɛ.Dot[A]) ?=> (group: Ɛ.Group[A]) ?=> (action: group.Action[X]) ?=>
              val nameOfArrow: UNIT ~> A =
                Ɛ.RichArrow:
                  dot[X].arrow.name
                \ action.toExponent
              val nameOfInverse: UNIT ~> A =
                group.inverse compose nameOfArrow
              val constantArrow: A ~> A =
                nameOfArrow compose Ɛ.toUnit[A]
              val constantInverse: A ~> A =
                nameOfInverse compose Ɛ.toUnit[A]
              val idA: A~> A =
                Ɛ.id[A]
              def biMultiply(
                prefix: A ~> A,
                suffix: A ~> A
              ): A ~> A =
                group.multiply compose :
                  Ɛ.RichArrow(
                    group.multiply compose :
                      Ɛ.RichArrow(prefix) x idA
                  ) x suffix
              withDot(
                Ɛ.Automorphism(
                  biMultiply(constantInverse, constantArrow),
                  biMultiply(constantArrow, constantInverse)
                )
              ):
                implicit val autoGroup: Group[A] =
                  Group[A](
                    unit = group.unit,
                    multiply = group.multiply,
                    inverse = group.inverse
                  )
                autoGroup.Action[X](action.actionMultiply).preserving(
                  dot[X].arrow
                ):
                  [P] => (_ : Dot[P]) ?=> (_: Group[P]) ?=> (_: P ~> A) =>
                  given autoGroup.Action[X] =
                    autoGroup.Action[X]:
                      action.actionMultiply
                  block[A]

      override val optionator: Optionator =
        new Optionator:
          override type OPTION[X] = Ɛ.OPTION[X]
          override def partialArrowClassifier[
            X: Dot
          ]: PartialArrowClassifier[X, OPTION[X]] =
            val delegate: Ɛ.PartialArrowClassifier[X, OPTION[X]] =
              Ɛ.optionator.partialArrowClassifier[X]
            def liftAuto(
              auto: X ~> X
            ): OPTION[X] ~> OPTION[X] =
              delegate.extendAlong[X, OPTION[X]](
                delegate.some,
                auto
              )
            withDot(
                new Ɛ.Automorphism[OPTION[X]](
                  liftAuto:
                    dot[X].arrow,
                  liftAuto:
                    dot[X].inverse
                )
            ):
              new PartialArrowClassifier[X, OPTION[X]]:
                override val some: X ~> OPTION[X] =
                  x => delegate.some(x)
                override val none: UNIT ~> OPTION[X] =
                  delegate.none
                override def extendAlong[V: Dot, W: Dot](
                  monic: V ~> W,
                  arrow: V ~> X
                ): W ~> OPTION[X] =
                  delegate.extendAlong[V, W](
                    monic,
                    arrow
                  )
