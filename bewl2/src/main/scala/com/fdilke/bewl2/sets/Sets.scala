package com.fdilke.bewl2.sets

import com.fdilke.bewl2.Topos
import com.fdilke.bewl2.Mappable
import com.fdilke.bewl2.sets.SetsUtilities.allMaps

implicit object Sets extends Topos[
  Set, [A] =>> A, Void, Unit, Map
]:
  override def equalArrows[X: Set, Y: Set](
   f1: X => Y,
   f2: X => Y
 ): Boolean =
    implicitly[Set[X]].forall { x =>
      f1(x) == f2(x)
    }

  override def uncachedProductObject[
    X: Set,
    Y: Set
  ]: Set[(X, Y)] =
    for {
      x <- dot[X]
      y <- dot[Y]
    } yield
      (x, y)

  override def productMagic[A: Set, B: Set](
    a: A,
    b: B
  ): (A, B) =
    (a, b)

  override def uncachedExponentialObject[
    X: Set,
    Y: Set
  ]: Set[X Map Y] =
    allMaps(dot[X], dot[Y]).toSet

  override def sanityTest[X: Set]: Unit = ()
  
  override def sanityTest[X: Set, Y: Set](
    f: X ~> Y
  ): Unit =
    dot[X].foreach { x =>
      if (!dot[Y].contains(f(x)))
        throw new IllegalArgumentException(s"target outside range: ${f(x)} not found in ${dot[Y]}")
    }

  override implicit val unitDot: Set[Unit] = Set(())
  override implicit val zeroDot: Set[Void] = Set.empty

  override def fromZero[X: Set]: Void => X = { _ =>
    throw new IllegalArgumentException("Encountered a VOID")
  }

  override def toUnit[X: Set]: X => Unit = {
    _ => ()
  }

  override def evaluation[X: Set, Y: Set]: ((X Map Y, X)) => Y =
    (xMapY, x) => xMapY(x)

  override def transpose[X: Set, Y: Set, Z: Set](
    xy2z: ((X, Y)) => Z
  ): X => (Y Map Z) =
    x => Map.from(
      implicitly[Set[Y]] map { y =>
        y -> xy2z( (x, y) )
      }
    )

  override def doEqualizer[X: Set, Y: Set, RESULT](
    f: X => Y,
    f2: X => Y
  )(
    capture: [A] => Equalizer[A, X] => Set[A] ?=> RESULT
  ): RESULT =
    inline def maskType[A](
      dotA: Set[A],
      theInclusion: A => X,
      theRestriction: [R] => (R ~> X) => Set[R] ?=> (R ~> A)
    ): RESULT =
      given Set[A] = dotA
      capture[A](
        new Equalizer[A, X] {
          override val inclusion: A ~> X =
            theInclusion
          override def restrict[R: Set](
            arrow: R ~> X
          ): R ~> A =
            theRestriction(arrow)
        }
      )
    type X_ = X
    val whereEqual: Set[X_] =
      summon[Set[X]] filter { x =>
        f(x) == f2(x)
      }
    maskType[X_](
      dotA =whereEqual,
      theInclusion = identity,
      theRestriction = [R] => (arrow: R ~> X) => (rr: Set[R]) ?=> {
        (r: R) => arrow(r)
      }
    )



  

  