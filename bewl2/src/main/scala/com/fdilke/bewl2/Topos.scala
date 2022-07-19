package com.fdilke.bewl2

import scala.annotation.targetName
import com.fdilke.bewl2.Mappable

trait BaseTopos[
  SET[_],
  CTXT[_] : Mappable
]:

  val mappable: Mappable[CTXT] =
    Mappable[CTXT]

  @targetName("topos arrow")
  type ~>[X, Y] = CTXT[X] => CTXT[Y]

  def equalArrows[X: SET, Y: SET](
    f1: X ~> Y,
    f2: X ~> Y
  ): Boolean

  def uncachedProductObject[
    X: SET,
    Y: SET
  ]: SET[(X, Y)]

  def productMagic[A: SET, B: SET](
    ca: CTXT[A],
    cb: CTXT[B]
  ): CTXT[(A, B)]

  def sanityTest[X: SET]: Unit
  def sanityTest[X: SET, Y: SET](f: X ~> Y): Unit

trait Topos[
  SET[_],
  CTXT[_]: Mappable
] extends BaseTopos[SET, CTXT]:

  final inline def arrow[X: SET, Y: SET]( // necessary?
    f: CTXT[X] => CTXT[Y]
  ): X ~> Y =
    f

  final def dot[X: SET]: SET[X] =
    implicitly[SET[X]]

  final def id[X: SET]: X ~> X =
    identity[CTXT[X]]

  final def π0[X, Y]: (X, Y) ~> X =
    c => mappable.map(c, _._1)

  final def π1[X, Y]: (X, Y) ~> Y =
    c => mappable.map(c, _._2)

  implicit def productObject[
      X: SET,
      Y: SET
  ]: SET[(X, Y)] = {
    uncachedProductObject[X, Y]
//    ??? // add caching!
  }

  implicit final class ArrowHelpers[X: SET, Y: SET](
    f: X ~> Y
  ):
    @targetName("arrow equality")
    def =!=(
      f2: X ~> Y
    ): Boolean =
      equalArrows(f, f2)

    @targetName("arrow composition")
    final def o[V: SET](
      f2: V ~> X
    ): V ~> Y =
      f compose f2

    @targetName("arrow multiplication")
    final def x[Z: SET](
     f2: X ~> Z
    ): X ~> (Y, Z) =
      cx => productMagic[Y, Z](
        f(cx), f2(cx)
      )

    final def sanityTest: Unit =
      Topos.this.sanityTest[X, Y](f)

object Topos:
  def apply[
    SET[_],
    CTXT[A]: Mappable
  ](
   implicit topos: Topos[SET, CTXT]
  ): Topos[SET, CTXT] =
    topos
