package com.fdilke.bewl2

import scala.annotation.targetName
import com.fdilke.bewl2.Mappable
import com.fdilke.bewl2.helper.Memoize

trait BaseTopos[
  DOT[_],
  CTXT[_] : Mappable,
  VOID,
  UNIT,
  →[_, _]
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
  ]: DOT[X → Y]

  def productMagic[A: DOT, B: DOT](
    ca: CTXT[A],
    cb: CTXT[B]
  ): CTXT[(A, B)]

  def sanityTest[X: DOT]: Unit
  def sanityTest[X: DOT, Y: DOT](f: X ~> Y): Unit

  implicit val unitDot: DOT[UNIT]
  implicit val zeroDot: DOT[VOID]
  def toUnit[X: DOT]: X ~> UNIT
  def fromZero[X: DOT]: VOID ~> X

  def evaluation[X: DOT, Y: DOT]: (X → Y, X) ~> Y
  def transpose[X: DOT, Y: DOT, Z: DOT](
    xy2z: (X, Y) ~> Z
  ): X ~> (Y → Z)

trait Topos[
  DOT[_],
  CTXT[_]: Mappable,
  VOID,
  UNIT,
  →[_, _]
] extends BaseTopos[DOT, CTXT, VOID, UNIT, →]:

  final inline def arrow[X: DOT, Y: DOT]( // necessary?
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
    [X, Y] => ((DOT[X], DOT[Y])) => DOT[X → Y]
  = Memoize[
      [X, Y] =>> (DOT[X], DOT[Y]),
      [X, Y] =>> DOT[X → Y]
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
  ]: DOT[X → Y] =
    memoizedExponential[X, Y](
      dot[X],
      dot[Y]
    )

  implicit final class ArrowHelpers[X: DOT, Y: DOT](
    f: X ~> Y
  ):
    @targetName("arrow equality")
    def =!=(
      f2: X ~> Y
    ): Boolean =
      equalArrows(f, f2)

    @targetName("arrow composition")
    final def o[V: DOT](
      f2: V ~> X
    ): V ~> Y =
      f compose f2

    @targetName("arrow multiplication")
    final def x[Z: DOT](
     f2: X ~> Z
    ): X ~> (Y, Z) =
      cx => productMagic[Y, Z](
        f(cx), f2(cx)
      )

    final def sanityTest: Unit =
      Topos.this.sanityTest[X, Y](f)

object Topos:
  inline def apply[
    DOT[_],
    CTXT[A]: Mappable,
    VOID,
    UNIT,
    →[_, _]
  ](
   implicit topos: Topos[DOT, CTXT, VOID, UNIT, →]
  ): Topos[DOT, CTXT, VOID, UNIT, →] =
    topos
