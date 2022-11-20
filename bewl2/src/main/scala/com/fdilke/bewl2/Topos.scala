package com.fdilke.bewl2

import scala.annotation.targetName
import com.fdilke.bewl2.Mappable
import com.fdilke.bewl2.algebra.AlgebraicMachinery
import com.fdilke.bewl2.helper.Memoize
import Mappable.*
import com.fdilke.bewl2.logic.LogicalOperations

trait BaseTopos[
  DOT[_],
  CTXT[_] : Mappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
]:
  @targetName("topos arrow")
  type ~>[X, Y] = CTXT[X] => CTXT[Y]

  trait RawEqualizer[A, X]:
    val inclusion: A ~> X
    def restrict[R](
      dotR: DOT[R],
      arrow: R ~> X
    ): R ~> A

  def rawEqualArrows[X, Y](
    dotX: DOT[X],
    dotY: DOT[Y],
    f1: X ~> Y,
    f2: X ~> Y
  ): Boolean

  def uncachedProductObject[X, Y](
    dotX: DOT[X],
    dotY: DOT[Y],
  ): DOT[(X, Y)]

  def uncachedExponentialObject[X, Y](
    dotX: DOT[X],
    dotY: DOT[Y],
  ): DOT[X > Y]

  def rawProductMagic[X, Y](
    dotX: DOT[X],
    dotY: DOT[Y],
    ca: CTXT[X],
    cb: CTXT[Y]
  ): CTXT[(X, Y)]

  def rawSanityTest[X](dotX: DOT[X]): Unit
  def rawSanityTest[X, Y](
    dotX: DOT[X],
    dotY: DOT[Y],
    f: X ~> Y
  ): Unit

  def rawUnitDot: DOT[UNIT]
  def rawZeroDot: DOT[VOID]
  def rawOmegaDot: DOT[BEWL]
  def rawToUnit[X](dotX: DOT[X]): X ~> UNIT
  def rawFromZero[X](dotX: DOT[X]): VOID ~> X
  val truth: UNIT ~> BEWL
  def rawEvaluation[X, Y](dotX: DOT[X], dotY: DOT[Y]): (X > Y, X) ~> Y
  def rawTranspose[X, Y, Z](
    dotX: DOT[X],
    dotY: DOT[Y],
    dotZ: DOT[Z],
    xy2z: (X, Y) ~> Z
  ): X ~> (Y > Z)
  def rawDoEqualizer[X, Y, RESULT](
    dotX: DOT[X],
    dotY: DOT[Y],
    f: X ~> Y,
    f2: X ~> Y
  )(
    capture: [A] => RawEqualizer[A, X] => DOT[A] => RESULT
  ): RESULT
  def rawChiForMonic[X, Y](
    dotX: DOT[X],
    dotY: DOT[Y],
    monic: X ~> Y
  ): Y ~> BEWL
  def rawBackDivideMonic[X, Y, A](
    dotX: DOT[X],
    dotY: DOT[Y],
    dotA: DOT[A],
    arrow: X ~> Y,
    monic: A ~> Y
  ): X ~> A

trait Topos[
  DOT[_],
  CTXT[_]: Mappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
] extends BaseTopos[DOT, CTXT, VOID, UNIT, BEWL, >]
  with AlgebraicMachinery[DOT, CTXT, VOID, UNIT, BEWL, >]
  with LogicalOperations[DOT, CTXT, VOID, UNIT, BEWL, >]:

  val mappable: Mappable[CTXT] =
    Mappable[CTXT]

  trait Equalizer[A, X]:
    val inclusion: A ~> X
    def restrict[R: Dot](
      arrow: R ~> X
    ): R ~> A

  class Dot[X]( // TODO: can we have something like "Dot[X] protected("?
    val dot: DOT[X]
  ) {

  } // TODO: protect the constructor

  final def withDot[X, RESULT](
    dot: DOT[X]
  )(
    block: Dot[X] ?=> RESULT
  ): RESULT =
    implicit val _: Dot[X] = Dot(dot)
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

  final def withDotMask[X, RESULT]( // TODO: audit later. useful method? belongs elsewhere?
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
    rawEqualArrows(dot[X], dot[Y], f1, f2)

  final inline def productMagic[X: Dot, Y: Dot](
    ca: CTXT[X],
    cb: CTXT[Y]
  ): CTXT[(X, Y)] =
    rawProductMagic(dot[X], dot[Y], ca, cb)

  final inline def sanityTest[X: Dot]: Unit =
    rawSanityTest(dot[X])

  final inline def sanityTest[X: Dot, Y: Dot](
    f: X ~> Y
  ): Unit =
    rawSanityTest(dot[X], dot[Y], f)

  implicit val _: Dot[UNIT] = Dot(rawUnitDot)
  implicit val _: Dot[VOID] = Dot(rawZeroDot)
  implicit val _: Dot[BEWL] = Dot(rawOmegaDot)

  final inline def toUnit[X: Dot]: X ~> UNIT =
    rawToUnit(dot[X])

  final inline def fromZero[X: Dot]: VOID ~> X =
    rawFromZero(dot[X])

  final inline def evaluation[X: Dot, Y: Dot]: (X > Y, X) ~> Y =
    rawEvaluation(dot[X], dot[Y])

  final inline def transpose[X: Dot, Y: Dot, Z: Dot](
    xy2z: (X, Y) ~> Z
  ): X ~> (Y > Z) =
    rawTranspose(dot[X], dot[Y], dot[Z], xy2z)

  final def doEqualizer[X: Dot, Y: Dot, RESULT](
    f: X ~> Y,
    f2: X ~> Y
  )(
    capture: [A] => Equalizer[A, X] => Dot[A] ?=> RESULT
  ): RESULT =
    rawDoEqualizer(dot[X], dot[Y], f, f2)(
      [A] => (rawEqualizer: RawEqualizer[A, X]) => (dotA: DOT[A]) => {
        implicit val _: Dot[A] = Dot(dotA)
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
    rawChiForMonic(dot[X], dot[Y], monic)

  final inline def backDivideMonic[X: Dot, Y: Dot, A: Dot](
    arrow: X ~> Y,
    monic: A ~> Y
  ): X ~> A =
    rawBackDivideMonic(dot[X], dot[Y], dot[A], arrow, monic)

  final inline def arrow[X: Dot, Y: Dot]( // TODO: do we need this?
    f: CTXT[X] => CTXT[Y]
  ): X ~> Y =
    f

  final inline def dot[X: Dot]: DOT[X] =
    implicitly[Dot[X]].dot

  final inline def id[X: Dot]: X ~> X =
    identity[CTXT[X]]

  final def π0[X, Y]: (X, Y) ~> X =
    c => mappable.map(c, _._1)

  final def π1[X, Y]: (X, Y) ~> Y =
    c => mappable.map(c, _._2)

  final def toTrue[X: Dot]: X ~> BEWL =
    truth o toUnit[X]

  private val memoizedProduct:
    [X, Y] => ((Dot[X], Dot[Y])) => Dot[(X, Y)]
  = Memoize[
      [X, Y] =>> (Dot[X], Dot[Y]),
      [X, Y] =>> Dot[(X, Y)]
    ](
      [X, Y] => (dots: (Dot[X], Dot[Y])) => Dot(
        uncachedProductObject[X, Y](
          dots._1.dot,
          dots._2.dot
        )
      )
    )

  implicit def productObject[
      X: Dot,
      Y: Dot
  ]: Dot[(X, Y)] =
    memoizedProduct[X, Y](
      summon[Dot[X]],
      summon[Dot[Y]]
    )

  private val memoizedExponential:
    [X, Y] => ((Dot[X], Dot[Y])) => Dot[X > Y]
  = Memoize[
      [X, Y] =>> (Dot[X], Dot[Y]),
      [X, Y] =>> Dot[X > Y]
    ](
      [X, Y] => (dots: (Dot[X], Dot[Y])) => Dot(
        uncachedExponentialObject[X, Y](
          dots._1.dot,
          dots._2.dot
        )
      )
    )

  implicit def exponentialObject[
      X: Dot,
      Y: Dot
  ]: Dot[X > Y] =
    memoizedExponential[X, Y](
      summon[Dot[X]],
      summon[Dot[Y]]
    )

  inline def applicate[X: Dot, T, Y: Dot](
    ctxt: CTXT[T]
  )(
    f: T => (X > Y, X)
  ): CTXT[Y] =
    evaluation[X, Y].apply(
      ctxt.map(f)
    )

  type BiArrow[X, Y, Z] = (X, Y) ~> Z

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
      cx => productMagic[Y, Z](
        f(cx), f2(cx)
      )

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

    final def name: UNIT ~> (X > Y) =
      val whiffle: ((UNIT, X)) ~> X = π1[UNIT, X]
      // val piffle: ((UNIT, X)) ~> Y = f o whiffle
      transpose(f o whiffle)

// TODO: ^ sort this out

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
