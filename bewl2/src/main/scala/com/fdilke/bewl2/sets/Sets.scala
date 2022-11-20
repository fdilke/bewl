package com.fdilke.bewl2.sets

import com.fdilke.bewl2.Topos
import com.fdilke.bewl2.Mappable
import com.fdilke.bewl2.sets.SetsUtilities.allMaps

import scala.language.postfixOps

implicit object Sets extends Topos[
  Set, [A] =>> A, Void, Unit, Boolean, Map
]:
  override def rawEqualArrows[X, Y](
    dotX: Set[X],
    dotY: Set[Y],
    f1: X => Y,
    f2: X => Y
 ): Boolean =
    dotX.forall { x =>
      f1(x) == f2(x)
    }

  override def uncachedProductObject[X, Y](
    dotX: Set[X],
    dotY: Set[Y]
  ): Set[(X, Y)] =
    for {
      x <- dotX
      y <- dotY
    } yield
      (x, y)

  override def rawProductMagic[X, Y](
    dotX: Set[X],
    dotY: Set[Y],
    x: X,
    y: Y
  ): (X, Y) =
    (x, y)

  override def uncachedExponentialObject[X, Y](
    dotX: Set[X],
    dotY: Set[Y]
  ): Set[X Map Y] =
    allMaps(dotX, dotY).toSet

  override def rawSanityTest[X](
    dotX: Set[X]
  ): Unit = ()
  
  override def rawSanityTest[X, Y](
    dotX: Set[X],
    dotY: Set[Y],
    f: X ~> Y
  ): Unit =
    dotX.foreach { x =>
      if (!dotY.contains(f(x)))
        throw new IllegalArgumentException(s"target outside range: ${f(x)} not found in $dotY")
    }

  override def rawUnitDot: Set[Unit] = Set(())
  override def rawZeroDot: Set[Void] = Set.empty
  override def rawOmegaDot: Set[Boolean] = Set(true, false)
  override val truth: Unit => Boolean = _ => true

  override def rawFromZero[X](dotX: Set[X]): Void => X = { _ =>
    throw new IllegalArgumentException("Encountered a VOID")
  }

  override def rawToUnit[X](
    dotX: Set[X]
  ): X => Unit = {
    _ => ()
  }

  override def rawEvaluation[X, Y](
    dotX: Set[X],
    dotY: Set[Y]
  ): ((X Map Y, X)) => Y =
    (xMapY, x) => xMapY(x)

  override def rawTranspose[X, Y, Z](
    dotX: Set[X],
    dotY: Set[Y],
    dotZ: Set[Z],
    xy2z: ((X, Y)) => Z
  ): X => (Y Map Z) =
    x => Map.from(
      dotY map { y =>
        y -> xy2z( (x, y) )
      }
    )

  override def rawDoEqualizer[X, Y, RESULT](
    dotX: Set[X],
    dotY: Set[Y],
    f: X => Y,
    f2: X => Y
  )(
    capture: [A] => RawEqualizer[A, X] => Set[A] => RESULT
  ): RESULT =
    inline def maskType[A](
      dotA: Set[A],
      theInclusion: A => X,
      theRestriction: [R] => (R ~> X) => Set[R] => (R ~> A)
    ): RESULT =
      capture[A](
        new RawEqualizer[A, X] {
          override val inclusion: A ~> X =
            theInclusion

          override def restrict[R](
            dotR: Set[R],
            arrow: R ~> X
          ): R ~> A =
            theRestriction(arrow)(dotR)
        }
      )(dotA)

    type X_ = X
    val whereEqual: Set[X_] =
      dotX.filter { x =>
        f(x) == f2(x)
      }
    maskType[X_](
      dotA = whereEqual,
      theInclusion = identity,
      theRestriction = [R] => (arrow: R ~> X) => (rr: Set[R]) => {
        (r: R) => arrow(r)
      }
    )

  override def rawChiForMonic[X, Y](
    dotX: Set[X],
    dotY: Set[Y],
    monic: X => Y
  ): Y => Boolean =
    y => dotX.exists { x =>
      monic(x) == y
    }

  override def rawBackDivideMonic[X, Y, A](
    dotX: Set[X],
    dotY: Set[Y],
    dotA: Set[A],
    arrow: X => Y,
    monic: A => Y
  ): X => A =
    x => {
      val target: Y = arrow(x)
      dotA.find { a =>
        monic(a) == target
      } get
    }




  

  