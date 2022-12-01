package com.fdilke.bewl2.sets

import com.fdilke.bewl2.PreTopos
import com.fdilke.bewl2.Topos
import com.fdilke.bewl2.Mappable
import com.fdilke.bewl2.sets.SetsUtilities.allMaps

import scala.language.postfixOps

object PreSets extends PreTopos[Set, [A] =>> A, Void, Unit, Boolean, Map]:
  override def equalArrows[X, Y](
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

  override def productMagic[X, Y](
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

  override def sanityTest[X](
    dotX: Set[X]
  ): Unit = ()
  
  override def sanityTest[X, Y](
    dotX: Set[X],
    dotY: Set[Y],
    f: X ~> Y
  ): Unit =
    dotX.foreach { x =>
      if (!dotY.contains(f(x)))
        throw new IllegalArgumentException(s"target outside range: ${f(x)} not found in $dotY")
    }

  override val unitDot: Set[Unit] = Set(())
  override val zeroDot: Set[Void] = Set.empty
  override val omegaDot: Set[Boolean] = Set(true, false)
  override val truth: Unit => Boolean = _ => true

  override def fromZero[X](dotX: Set[X]): Void => X = { _ =>
    throw new IllegalArgumentException("Encountered a VOID")
  }

  override def toUnit[X](
    dotX: Set[X]
  ): X => Unit = {
    _ => ()
  }

  override def evaluation[X, Y](
    dotX: Set[X],
    dotY: Set[Y]
  ): ((X Map Y, X)) => Y =
    (xMapY, x) => xMapY(x)

  override def transpose[X, Y, Z](
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

  override def doEqualizer[X, Y, RESULT](
    dotX: Set[X],
    dotY: Set[Y],
    f: X => Y,
    f2: X => Y
  )(
    capture: [A] => RawEqualizer[A, X] => Set[A] => RESULT
  ): RESULT =
    type X_ = X
    val whereEqual: Set[X_] =
      dotX.filter { x =>
        f(x) == f2(x)
      }

    val theRestriction: [R] => (R ~> X) => Set[R] => (R ~> X) =
      [R] => (arrow: R ~> X) => (rr: Set[R]) => { (r: R) => arrow(r) }
    val xEqualsX =
        new RawEqualizer[X, X] {
          override val inclusion: X ~> X =
            identity[X]

          override def restrict[R](
            dotR: Set[R],
            arrow: R ~> X
          ): R ~> X =
            theRestriction(arrow)(dotR)
        }
    capture[X](xEqualsX)(whereEqual)

  override def chiForMonic[X, Y](
    dotX: Set[X],
    dotY: Set[Y],
    monic: X => Y
  ): Y => Boolean =
    y => dotX.exists { x =>
      monic(x) == y
    }

  override def backDivideMonic[X, Y, A](
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

object Sets extends Topos[
  Set, [A] =>> A, Void, Unit, Boolean, Map
](PreSets)

  

  