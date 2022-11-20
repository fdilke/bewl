package com.fdilke.bewl2

import scala.annotation.targetName

trait PreTopos[
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

  val rawUnitDot: DOT[UNIT]
  val rawZeroDot: DOT[VOID]
  val rawOmegaDot: DOT[BEWL]
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

