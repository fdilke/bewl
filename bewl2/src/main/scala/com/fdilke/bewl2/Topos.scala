package com.fdilke.bewl2

import scala.annotation.targetName
import com.fdilke.bewl2.Mappable
import com.fdilke.bewl2.algebra.AlgebraicMachinery
import com.fdilke.bewl2.helper.Memoize
import Mappable.*
import com.fdilke.bewl2.logic.LogicalOperations

class Topos[
  DOT[_],
  CTXT[_]: Mappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
](pretopos: PreTopos[DOT, CTXT, VOID, UNIT, BEWL, >])
  extends AlgebraicMachinery[DOT, CTXT, VOID, UNIT, BEWL, >]
  with LogicalOperations[DOT, CTXT, VOID, UNIT, BEWL, >]:
  Ɛ =>

  @targetName("topos arrow")
  type ~>[X, Y] = CTXT[X] => CTXT[Y]

  trait Equalizer[A, X]:
    val inclusion: A ~> X
    def restrict[R: Dot](
      arrow: R ~> X
    ): R ~> A

  class Dot[X] private[Topos](
    val dot: DOT[X]
  ) {
    private[Topos] val memoizedProduct:
      [Y] => Dot[Y] => Dot[(X, Y)]
    = Memoize.type1[
      [Y] =>> Dot[Y],
      [Y] =>> Dot[(X, Y)]
    ](
      [Y] => (dotY: Dot[Y]) => Dot(
        pretopos.uncachedProductObject[X, Y](
          dot,
          dotY.dot
        )
      )
    )

    private[Topos] val memoizedExponential:
      [Y] => Dot[Y] => Dot[X > Y]
    = Memoize.type1[
      [Y] =>> Dot[Y],
      [Y] =>> Dot[X > Y]
    ](
      [Y] => (dotY: Dot[Y]) => Dot(
        pretopos.uncachedExponentialObject[X, Y](
          dot,
          dotY.dot
        )
      )
    )

    lazy val diagonal: (X, X) ~> BEWL =
      given Dot[X] = this
      { (x: CTXT[X]) => x ⊕ x }.chi
  }

  final def withDot[X, RESULT](
    dot: DOT[X]
  )(
    block: Dot[X] ?=> RESULT
  ): RESULT =
    given Dot[X] = Dot(dot)
    block

  final inline def withDots[X, Y, RESULT](
    dotX: DOT[X],
    dotY: DOT[Y]
  )(
    block: Dot[X] ?=> Dot[Y] ?=> RESULT
  ): RESULT =
    withDot(dotX) {
      withDot(dotY) {
        block
      }
    }

  final inline def withDots[X, Y, Z, RESULT](
    dotX: DOT[X],
    dotY: DOT[Y],
    dotZ: DOT[Z]
  )(
    block: Dot[X] ?=> Dot[Y] ?=> Dot[Z] ?=> RESULT
  ): RESULT =
    withDot(dotX) {
      withDot(dotY) {
        withDot(dotZ) {
          block
        }
      }
    }

  final def withDotMask[X, RESULT](
    dot: DOT[X]
  )(
    block: [X_] => Dot[X_] ?=> (X_ =:= X) ?=> (X =:= X_) ?=> RESULT
  ): RESULT =
    withDot(dot) {
      block[X]
    }

  final inline def equalArrows[X: Dot, Y: Dot](
    f1: X ~> Y,
    f2: X ~> Y
  ): Boolean =
    pretopos.equalArrows(dot[X], dot[Y], f1, f2)

  final inline def sanityTest[X: Dot]: Unit =
    pretopos.sanityTest(dot[X])

  final inline def sanityTest[X: Dot, Y: Dot](
    f: X ~> Y
  ): Unit =
    pretopos.sanityTest(dot[X], dot[Y], f)

  given Dot[UNIT] = Dot(pretopos.unitDot)
  given Dot[VOID] = Dot(pretopos.zeroDot)
  given Dot[BEWL] = Dot(pretopos.omegaDot)

  final inline def toUnit[X: Dot]: X ~> UNIT =
    pretopos.toUnit(dot[X])

  final inline def fromZero[X: Dot]: VOID ~> X =
    pretopos.fromZero(dot[X])

  final inline def evaluation[X: Dot, Y: Dot]: (X > Y, X) ~> Y =
    pretopos.evaluation(dot[X], dot[Y])

  final inline def transpose[X: Dot, Y: Dot, Z: Dot](
    xy2z: (X, Y) ~> Z
  ): X ~> (Y > Z) =
    pretopos.transpose(dot[X], dot[Y], dot[Z], xy2z)

  final def doEqualizer[X: Dot, Y: Dot, RESULT](
    f: X ~> Y,
    f2: X ~> Y
  )(
    capture: [A] => Equalizer[A, X] => Dot[A] ?=> RESULT
  ): RESULT =
    pretopos.doEqualizer(dot[X], dot[Y], f, f2)(
      [A] => (rawEqualizer: pretopos.RawEqualizer[A, X]) => (dotA: DOT[A]) => {
        given Dot[A] = Dot(dotA)
        capture[A](new Equalizer[A, X] {
          override val inclusion: A ~> X =
            rawEqualizer.inclusion

          override def restrict[R: Dot](
            arrow: R ~> X
          ): R ~> A =
            rawEqualizer.restrict(
              dot[R],
              arrow
            )
        })
      }
    )

  final inline def chiForMonic[X: Dot, Y: Dot](
    monic: X ~> Y
  ): Y ~> BEWL =
    pretopos.chiForMonic(dot[X], dot[Y], monic)

  final inline def backDivideMonic[X: Dot, Y: Dot, A: Dot](
    arrow: X ~> Y,
    monic: A ~> Y
  ): X ~> A =
    pretopos.backDivideMonic(dot[X], dot[Y], dot[A], arrow, monic)

  final inline def dot[X: Dot]: DOT[X] =
    implicitly[Dot[X]].dot

  final inline def id[X: Dot]: X ~> X =
    identity[CTXT[X]]

  final def π0[X, Y]: (X, Y) ~> X =
    _.map { _._1 }

  final def π1[X, Y]: (X, Y) ~> Y =
    _.map { _._2 }

  final def toTrue[X: Dot]: X ~> BEWL =
    truth o toUnit[X]

  final def =?=[X: Dot]: (X, X) ~> BEWL =
    summon[Dot[X]].diagonal

  final val truth: NullaryOp[BEWL] =
    pretopos.truth

  implicit def productObject[
      X: Dot,
      Y: Dot
  ]: Dot[(X, Y)] =
    summon[Dot[X]].memoizedProduct[Y](
      summon[Dot[Y]]
    )

  implicit def exponentialObject[
      X: Dot,
      Y: Dot
  ]: Dot[X > Y] =
    summon[Dot[X]].memoizedExponential[Y](
      summon[Dot[Y]]
    )

  final def applicate[X: Dot, T, Y: Dot](
    ctxt: CTXT[T]
  )(
    f: T => (X > Y, X)
  ): CTXT[Y] =
    evaluation[X, Y].apply(
      ctxt.map(f)
    )

  final def ∀[X: Dot]: (X > BEWL) ~> BEWL =
    (truth o toUnit[X]).name.chi

  final def ∀[X: Dot, Y: Dot](
    f: (X, Y) ~> BEWL
  ): X ~> BEWL =
    ∀[Y] o transpose(f)

  object ⊕ :
    def unapply[X: Dot, Y: Dot](
      xy: CTXT[(X, Y)]
    ): Option[(CTXT[X], CTXT[Y])] =
      Some(xy.map{ _._1 }, xy.map { _._2 })

  extension[X: Dot](x: CTXT[X])
    def ⊕[Y: Dot](y: CTXT[Y]): CTXT[(X, Y)] =
      pretopos.productMagic(dot[X], dot[Y], x, y)

  type BiArrow[X, Y, Z] = (X, Y) ~> Z

  extension[X: Dot, Y: Dot, Z: Dot] (
    biArrow: BiArrow[X, Y, Z]
  )
    inline def apply[A: Dot](
      f: A ~> X,
      g: A ~> Y
    ): A ~> Z =
      biArrow o (f x g)

    inline def apply(
      x: CTXT[X],
      y: CTXT[Y]
    ): CTXT[Z] =
      biArrow(x ⊕ y)

  implicit final class RichArrow[X: Dot, Y: Dot](
    f: X ~> Y
  ):
    @targetName("arrow equality")
    inline final def =!=(
      f2: X ~> Y
    ): Boolean =
      equalArrows(f, f2)

    @targetName("arrow composition")
    inline final def o[V: Dot](
      f2: V ~> X
    ): V ~> Y =
      f compose f2

    @targetName("arrow multiplication")
    inline final def x[Z: Dot](
     f2: X ~> Z
    ): X ~> (Y, Z) =
      cx => f(cx) ⊕ f2(cx)

    @targetName("arrow equalizers")
    inline final def ?=[RESULT](
     f2: X ~> Y
    )(
      capture: [A] => Equalizer[A, X] => (zzz: Dot[A]) ?=> RESULT
    ): RESULT =
      doEqualizer(f, f2)(capture)

    @targetName("characteristic of a monic")
    inline final def chi: Y ~> BEWL =
      chiForMonic(f)

    @targetName("backdivision by a monic")
    inline final def \[A: Dot](
      monic: A ~> Y
    ): X ~> A =
      backDivideMonic(f, monic)
      
    @targetName("test for iso") // TODO: fix!!
    final def isIsoPlaceholderTrue: Boolean =
      true

    inline final def sanityTest: Unit =
      Topos.this.sanityTest[X, Y](f)

    final def name: NullaryOp[X > Y] =
      transpose(f o π1[UNIT, X])

object Topos:
  inline def apply[
    DOT[_],
    CTXT[A]: Mappable,
    VOID,
    UNIT,
    BEWL,
    >[_, _]
  ](
   implicit topos: Topos[DOT, CTXT, VOID, UNIT, BEWL, >]
  ): Topos[DOT, CTXT, VOID, UNIT, BEWL, >] =
    topos
