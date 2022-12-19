package com.fdilke.bewl2

import com.fdilke.bewl2.Mappable
import com.fdilke.bewl2.algebra.AlgebraicMachinery
import com.fdilke.bewl2.helper.Memoize
import Mappable.*
import com.fdilke.bewl2.logic.LogicalOperations
import com.fdilke.utility.Mask

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

  type ~>[X, Y] = CTXT[X] => CTXT[Y]
  type BiArrow[X, Y, Z] = (X, Y) ~> Z
  type UntupledBiArrow[P, Q, X] = (CTXT[P], CTXT[Q]) => CTXT[X]

  trait Equalizer[A, X]:
    val inclusion: A ~> X
    def restrict[R: Dot](
      arrow: R ~> X
    ): R ~> A
    final inline def restrict[P: Dot, Q: Dot](
      untupledArrow: UntupledBiArrow[P, Q, X]
    ): (P, Q) ~> A =
      restrict[(P, Q)] {
        case p ⊕ q => untupledArrow(p, q)
      }

  class Dot[X] private[Topos](
    val dot: DOT[X]
  ) {
    given Dot[X] = this

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
      { (x: CTXT[X]) => x ⊕ x }.chi

    final def ∃ : (X > BEWL) ~> BEWL =
      val evalPredicate: BiArrow[X > BEWL, X, BEWL] =
        evaluation[X, BEWL]
      Ɛ.∀[X > BEWL, BEWL]{ (f, ω) =>
        (Ɛ.∀[BEWL, X] { (χ, x) =>
          evalPredicate(f, x) → χ
        }: BEWL ~> BEWL) (ω) → ω
      }

    final def ∀ : (X > BEWL) ~> BEWL =
      (truth o toUnit[X]).name.chi

    lazy val singleton: X ~> (X > BEWL) =
      transpose[X, X, BEWL]{
        // case x ⊕ y => x =?= y
        =?=[X]
      }

    lazy val pac: PartialArrowClassifier[X, _] =
      PartialArrowClassifier[X]
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
    capture: [A] => Dot[A] ?=> Equalizer[A, X] => RESULT
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

  final def =?=[X: Dot]: BiArrow[X, X, BEWL] =
    summon[Dot[X]].diagonal

  final val truth: NullaryOp[BEWL] =
    pretopos.truth

  final def morphisms[X: Dot, Y: Dot]: Iterable[X ~> Y] =
    pretopos.enumerateMorphisms(dot[X], dot[Y])

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

  inline def withPac[X: Dot, RESULT](
    block: [OPTION_X] => Dot[OPTION_X] ?=> PartialArrowClassifier[X, OPTION_X] => RESULT
  ): RESULT =
    summon[Dot[X]].pac.withMe(block)

  // TODO: would require OPTION[_] to be a fixed type, may be able to finesse
  // final inline def some[X: Dot]: X ~> Option[X] =
  //   _.map { Some(_) }
  // final inline def none[X: Dot]: UNIT ~> Option[X] =
  //   summon[Dot[X]].pac.none
  // final inline def extendAlong[V: Dot, W: Dot, X: Dot](
  //   monic: V ~> W,
  //   arrow: V ~> X
  // ): W ~> Option[X] =
  //   summon[Dot[X]].pac.extendAlong[V, W](monic, arrow)
    
  final def applicate[X: Dot, T, Y: Dot](
    ctxt: CTXT[T]
  )(
    f: T => (X > Y, X)
  ): CTXT[Y] =
    evaluation[X, Y].apply(
      ctxt.map(f)
    )

  final def ∀[X: Dot](
    fn: X ~> BEWL
  ): Boolean =
    fn =!= (truth o toUnit[X])

  final def ∀[X: Dot]: (X > BEWL) ~> BEWL =
    summon[Dot[X]].∀

  final def ∀[X: Dot, Y: Dot](
    f: (X, Y) ~> BEWL
  ): X ~> BEWL =
    ∀[Y] o transpose(f)

  final inline def ∀[X: Dot, Y: Dot](
    untupledFn: UntupledBiArrow[X, Y, BEWL]
  ): X ~> BEWL =
    ∀[X, Y]{
      case x ⊕ y => untupledFn(x, y)
    }

  final def ∃[X: Dot](
    fn: X ~> BEWL
  ): Boolean =
    !(fn =!= (falsity o toUnit[X]))

  // given f: X -> omega then: "∃x: f(x)" is ∀ω (∀x: f(x) => ω) => ω"
  final def ∃[X: Dot]: (X > BEWL) ~> BEWL =
    summon[Dot[X]].∃

  final def ∃[X: Dot, Y: Dot](
    f: (X, Y) ~> BEWL
  ): X ~> BEWL =
    ∃[Y] o transpose(f)

  final inline def ∃[X: Dot, Y: Dot](
    untupledFn: UntupledBiArrow[X, Y, BEWL]
  ): X ~> BEWL =
    ∃[X, Y]{
      case x ⊕ y => untupledFn(x, y)
    }

  object ⊕ :
    def unapply[X: Dot, Y: Dot](
      xy: CTXT[(X, Y)]
    ): Option[(CTXT[X], CTXT[Y])] =
      Some(xy.map{ _._1 }, xy.map { _._2 })

  extension[X: Dot](x: CTXT[X])
    def ⊕[Y: Dot](y: CTXT[Y]): CTXT[(X, Y)] =
      pretopos.productMagic(dot[X], dot[Y], x, y)

    inline def =?=(x2: CTXT[X]): CTXT[BEWL] =
      (Ɛ.=?=[X]: BiArrow[X, X, BEWL])(x, x2)

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
    inline final def =!=(
      f2: X ~> Y
    ): Boolean =
      pretopos.equalArrows(dot[X], dot[Y], f, f2)

    inline final def o[V: Dot](
      f2: V ~> X
    ): V ~> Y =
      f compose f2

    inline final def x[Z: Dot](
     f2: X ~> Z
    ): X ~> (Y, Z) =
      cx => f(cx) ⊕ f2(cx)

    inline final def ?=[RESULT](
     f2: X ~> Y
    )(
      capture: [A] => Dot[A] ?=> Equalizer[A, X] => RESULT
    ): RESULT =
      doEqualizer(f, f2)(capture)

    inline final def chi: Y ~> BEWL =
      chiForMonic(f)

    inline final def \[A: Dot](
      monic: A ~> Y
    ): X ~> A =
      backDivideMonic(f, monic)
      
    final def isMonic: Boolean =
      ∀[(X, X)] { case x1 ⊕ x2 =>
        (f(x1) =?= f(x2)) → (x1 =?= x2)
      }

    final def isEpic: Boolean =
      epicVerifier.isEpic(f)

    final def isIso: Boolean =
      isMonic && isEpic

    inline final def sanityTest: Unit =
      Topos.this.sanityTest[X, Y](f)

    final def name: NullaryOp[X > Y] =
      transpose(f o π1[UNIT, X])

    final def factorize[RESULT](
      block: [I] => Dot[I] ?=> (epic: X ~> I, monic: I ~> Y) => RESULT
    ): RESULT =
      ∃[Y, X] { (y, x) =>
        y =?= f(x)
      } whereTrue { [I] => (_: Dot[I]) ?=> (equalizer: Equalizer[I, Y]) =>
        block[I](
          epic = equalizer.restrict(f),
          monic = equalizer.inclusion
        )
      }

  trait EpicVerifier:
    def isEpic[X: Dot, Y: Dot](
      arrow: X ~> Y
    ): Boolean

  class DefaultEpicVerifier extends EpicVerifier:
    override def isEpic[X: Dot, Y: Dot](
      arrow: X ~> Y
    ): Boolean =
      ∀[Y](
        ∃[Y, X] { (y, x) =>
          y =?= arrow(x)
        }
      )

  lazy val epicVerifier: EpicVerifier =
    new DefaultEpicVerifier

  inline final def singleton[X: Dot]: X ~> (X > BEWL) =
    summon[Dot[X]].singleton

  extension[X: Dot](f: X ~> BEWL)
    def whereTrue[RESULT](
      capture: [A] => Dot[A] ?=> Equalizer[A, X] => RESULT
    ) =
      f.?=(toTrue[X]) {
        capture
      }

  trait PartialArrowClassifier[X: Dot, OPTION_X: Dot]:
    final val classifier: Dot[OPTION_X] = summon[Dot[OPTION_X]]
    val some: X ~> OPTION_X
    val none: UNIT ~> OPTION_X
    def extendAlong[V: Dot, W: Dot](
      monic: V ~> W,
      arrow: V ~> X
    ): W ~> OPTION_X
    final def withMe[RESULT](
      block: [OX] => Dot[OX] ?=> PartialArrowClassifier[X, OX] => RESULT
    ): RESULT =
      block[OPTION_X](this)

  object PartialArrowClassifier:
    def apply[X: Dot]: PartialArrowClassifier[X, _] =
      val evalPredicate: BiArrow[X > BEWL, X, BEWL] =
        evaluation[X, BEWL]
      val isSubSingleton: (X > BEWL) ~> BEWL =
        ∀[X > BEWL, (X, X)] {
          case p ⊕ (x ⊕ y) =>
            (evalPredicate(p ⊕ x) ∧ evalPredicate(p ⊕ y)) → (x =?= y)
        }
      isSubSingleton.whereTrue { 
        [OX] => (dotOptionX : Dot[OX]) ?=> (equalizer : Equalizer[OX, X > BEWL]) =>
          new PartialArrowClassifier[X, OX]:
            override val some: X ~> OX = 
              equalizer.restrict(singleton[X])
            override val none: UNIT ~> OX = 
              equalizer.restrict(
                transpose[UNIT, X, BEWL] { case u ⊕ _ => falsity(u) }
              )
            override def extendAlong[V: Dot, W: Dot](
              monic: V ~> W,
              arrow: V ~> X
            ): W ~> OX =
              equalizer.restrict(
                transpose[W, X, BEWL] { case w ⊕ x =>
                  (∃[W, V] { case w ⊕ v  =>
                      ( arrow(v) =?= x ) ∧ ( monic(v) =?= w )
                  } : W ~> BEWL)(w)
                }
              )
      }

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
