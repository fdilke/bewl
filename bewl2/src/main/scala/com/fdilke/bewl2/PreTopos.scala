package com.fdilke.bewl2

trait PreTopos[
  DOT[_],
  CTXT[_] : ProductMappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
]:
  type ~>[X, Y] = CTXT[X] => CTXT[Y]

  trait RawEqualizer[A, X]:
    val inclusion: A ~> X
    def restrict[R](
      dotR: DOT[R],
      arrow: R ~> X
    ): R ~> A

  def equalArrows[X, Y](
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
    toolkitX: toolkitBuilder.TOOLKIT[X],
    dotY: DOT[Y],
    toolkitY: toolkitBuilder.TOOLKIT[Y]
  ): DOT[X > Y]

  def sanityTest[X](dotX: DOT[X]): Unit
  def sanityTest[X, Y](
    dotX: DOT[X],
    dotY: DOT[Y],
    f: X ~> Y
  ): Unit

  val unitDot: DOT[UNIT]
  val zeroDot: DOT[VOID]
  val omegaDot: DOT[BEWL]
  def toUnit[X](dotX: DOT[X]): X ~> UNIT
  def fromZero[X](dotX: DOT[X]): VOID ~> X
  val truth: UNIT ~> BEWL
  def enumerateMorphisms[X, Y](dotX: DOT[X], dotY: DOT[Y]): Iterable[X ~> Y]
  def evaluation[X, Y](dotX: DOT[X], dotY: DOT[Y]): (X > Y, X) ~> Y
  def transpose[X, Y, Z](
    dotX: DOT[X],
    dotY: DOT[Y],
    dotZ: DOT[Z],
    xy2z: (X, Y) ~> Z
  ): X ~> (Y > Z)
  def doEqualizer[X, Y, RESULT](
    dotX: DOT[X],
    dotY: DOT[Y],
    f: X ~> Y,
    f2: X ~> Y
  )(
    capture: [A] => RawEqualizer[A, X] => DOT[A] => RESULT
  ): RESULT
  def chiForMonic[X, Y](
    dotX: DOT[X],
    dotY: DOT[Y],
    monic: X ~> Y
  ): Y ~> BEWL
  def backDivideMonic[X, Y, A](
    dotX: DOT[X],
    dotY: DOT[Y],
    dotA: DOT[A],
    arrow: X ~> Y,
    monic: A ~> Y
  ): X ~> A

  trait ToolkitBuilder:
    type TOOLKIT[_]
    def buildToolkit[X](dot: DOT[X]): TOOLKIT[X]

  val toolkitBuilder: ToolkitBuilder =
    new ToolkitBuilder:
      type TOOLKIT[_] = Unit
      def buildToolkit[X](dot: DOT[X]): Unit = ()
