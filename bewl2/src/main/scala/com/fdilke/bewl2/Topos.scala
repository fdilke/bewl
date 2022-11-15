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
  val mappable: Mappable[CTXT] =
    Mappable[CTXT]

  @targetName("topos arrow")
  type ~>[X, Y] = CTXT[X] => CTXT[Y]

  def equalArrows[X: DOT, Y: DOT](
    f1: X ~> Y,
    f2: X ~> Y
  ): Boolean

  def uncachedProductObject[
    X: DOT,
    Y: DOT
  ]: DOT[(X, Y)]

  def uncachedExponentialObject[
    X: DOT,
    Y: DOT
  ]: DOT[X > Y]

  def productMagic[A: DOT, B: DOT](
    ca: CTXT[A],
    cb: CTXT[B]
  ): CTXT[(A, B)]

  def sanityTest[X: DOT]: Unit
  def sanityTest[X: DOT, Y: DOT](f: X ~> Y): Unit

  implicit val unitDot: DOT[UNIT]
  implicit val zeroDot: DOT[VOID]
  implicit val omegaDot: DOT[BEWL]
  def toUnit[X: DOT]: X ~> UNIT
  def fromZero[X: DOT]: VOID ~> X
  val truth: UNIT ~> BEWL

  def evaluation[X: DOT, Y: DOT]: (X > Y, X) ~> Y
  def transpose[X: DOT, Y: DOT, Z: DOT](
    xy2z: (X, Y) ~> Z
  ): X ~> (Y > Z)
  def doEqualizer[X: DOT, Y: DOT, RESULT](
    f: X ~> Y,
    f2: X ~> Y
  )(
    capture: [A] => Equalizer[A, X] => DOT[A] ?=> RESULT
  ): RESULT
  def chiForMonic[X: DOT, Y: DOT](
    monic: X ~> Y
  ): Y ~> BEWL
  def backDivideMonic[X: DOT, Y: DOT, A: DOT](
    arrow: X ~> Y,
    monic: A ~> Y
  ): X ~> A

  trait Equalizer[A : DOT, X : DOT]:
    val inclusion: A ~> X
    def restrict[R : DOT](
      arrow: R ~> X
    ): R ~> A


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

  final inline def arrow[X: DOT, Y: DOT]( // occasionally useful
    f: CTXT[X] => CTXT[Y]
  ): X ~> Y =
    f

  final inline def dot[X: DOT]: DOT[X] =
    implicitly[DOT[X]]

  final inline def id[X: DOT]: X ~> X =
    identity[CTXT[X]]

  final def π0[X, Y]: (X, Y) ~> X =
    c => mappable.map(c, _._1)

  final def π1[X, Y]: (X, Y) ~> Y =
    c => mappable.map(c, _._2)

  final def toTrue[X: DOT]: X ~> BEWL =
    truth o toUnit[X]

  private val memoizedProduct:
    [X, Y] => ((DOT[X], DOT[Y])) => DOT[(X, Y)]
  = Memoize[
      [X, Y] =>> (DOT[X], DOT[Y]),
      [X, Y] =>> DOT[(X, Y)]
    ](
      [X, Y] => (dots: (DOT[X], DOT[Y])) => {
        implicit val dotX: DOT[X] = dots._1
        implicit val dotY: DOT[Y] = dots._2
        uncachedProductObject[X, Y]
      }
    )

  implicit def productObject[
      X: DOT,
      Y: DOT
  ]: DOT[(X, Y)] =
    memoizedProduct[X, Y](
      dot[X],
      dot[Y]
    )

  private val memoizedExponential:
    [X, Y] => ((DOT[X], DOT[Y])) => DOT[X > Y]
  = Memoize[
      [X, Y] =>> (DOT[X], DOT[Y]),
      [X, Y] =>> DOT[X > Y]
    ](
      [X, Y] => (dots: (DOT[X], DOT[Y])) => {
        implicit val dotX: DOT[X] = dots._1
        implicit val dotY: DOT[Y] = dots._2
        uncachedExponentialObject[X, Y]
      }
    )

  implicit def exponentialObject[
      X: DOT,
      Y: DOT
  ]: DOT[X > Y] =
    memoizedExponential[X, Y](
      dot[X],
      dot[Y]
    )

  inline def applicate[X: DOT, T, Y: DOT](
    ctxt: CTXT[T]
  )(
    f: T => (X > Y, X)
  ): CTXT[Y] =
    evaluation[X, Y].apply(
      ctxt.map(f)
    )

  type BiArrow[X, Y, Z] = (X, Y) ~> Z
  
  implicit final class RichArrow[X: DOT, Y: DOT](
    f: X ~> Y
  ):
    @targetName("arrow equality")
    inline final def =!=(
      f2: X ~> Y
    ): Boolean =
      equalArrows(f, f2)

    @targetName("arrow composition")
    inline final def o[V: DOT](
      f2: V ~> X
    ): V ~> Y =
      f compose f2

    @targetName("arrow multiplication")
    inline final def x[Z: DOT](
     f2: X ~> Z
    ): X ~> (Y, Z) =
      cx => productMagic[Y, Z](
        f(cx), f2(cx)
      )

    @targetName("arrow equalizers")
    inline final def ?=[RESULT](
     f2: X ~> Y
    )(
      capture: [A] => Equalizer[A, X] => (zzz: DOT[A]) ?=> RESULT
    ): RESULT =
      doEqualizer(f, f2)(capture)

    @targetName("characteristic of a monic")
    inline final def chi: Y ~> BEWL =
      chiForMonic(f)

    @targetName("backdivision by a monic")
    inline final def \[A: DOT](
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

//  val foo2bar2baz: FOO ~> (BAR > BAZ) =
//    transpose(foobar2baz)


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
