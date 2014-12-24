package com.fdilke.bewl.actions

import com.fdilke.bewl.topos.{RichStarsAndQuivers, Topos, AlgebraicMachinery, BaseTopos}

// Monoids and monoid actions define as 'one-off' structures.
// TODO: once more general machinery is in place, update all monoid code and deprecate this

trait NaiveMonoidsAndActions { self: BaseTopos with AlgebraicMachinery with RichStarsAndQuivers =>
  case class NaiveMonoid[X <: ELEMENT](carrier: STAR[X], unit: NullaryOp[X], multiply: BinaryOp[X]) {
    def sanityTest {
      // check the left unit law
      if (carrier(carrier) {
        x => multiply(unit(carrier.toI(x)), x)
      } != carrier.identity)
        throw new IllegalArgumentException("Left unit law for * with unit 1")

      // check the right unit law
      if (carrier(carrier) {
        x => multiply(x, unit(carrier.toI(x)))
      } != carrier.identity)
        throw new IllegalArgumentException("Right unit law for * with unit 1")

      // check the associative law
      if ((carrier x carrier x carrier)(carrier) {
        case ((x, y), z) => multiply(x, multiply(y, z))
      } != (carrier x carrier x carrier)(carrier) {
        case ((x, y), z) => multiply(multiply(x, y), z)
      })
        throw new IllegalArgumentException("Associative law for *")
    }

    def rightAction[A <: ELEMENT](actionCarrier: STAR[A], actionMultiply: BiQuiver[A, X, A]) =
      new RightAction[A](actionCarrier, actionMultiply)

    lazy val rightActions : Topos =
      rightMonoidActions(this)

    class RightAction[A <: ELEMENT] (
      actionCarrier: STAR[A],
      actionMultiply: BiQuiver[A, X, A]
    ) {
      def sanityTest = {
        // check the right unit law
        if (actionCarrier(actionCarrier) {
          a => actionMultiply(a, unit(actionCarrier.toI(a)))
        } != actionCarrier.identity)
          throw new IllegalArgumentException("Right unit law for * with unit 1")

        // check the associative law
        if ((actionCarrier x carrier x carrier)(actionCarrier) {
          case ((a, x), y) => actionMultiply(a, multiply(x, y))
        } != (actionCarrier x carrier x carrier)(actionCarrier) {
          case ((a, x), y) => actionMultiply(actionMultiply(a, x), y)
        })
          throw new IllegalArgumentException("Associative law for monoid action *")
      }
    }
  }

  private def rightMonoidActions[SCALAR <: ELEMENT](monoid: NaiveMonoid[SCALAR]) =
    null.asInstanceOf[Topos]
/*
  private class RightMonoidActionsTopos[SCALAR <: ELEMENT](
    monoid: NaiveMonoid[SCALAR]
  ) extends Topos {
        override type ELEMENT = self.ELEMENT
        override type STAR[X <: ELEMENT] = RightActionStar[X]
        override type QUIVER[S <: ELEMENT, T <: ELEMENT] = RightActionQuiver[S, T]
        override type UNIT = self.UNIT
        override type TRUTH = SCALAR > self.TRUTH
        override val I =
          new RightActionStar[UNIT](monoid.rightAction(self.I,
            (self.I x monoid.carrier).biQuiver(self.I) {
              (i, m) => i
            }))
        override val omega = {
          val possibleIdeals = monoid.carrier.power
          val isIdeal = (monoid.carrier x monoid.carrier).forAll(possibleIdeals) {
            case (f, (m, n)) => TruthObject.implies(f(m), f(monoid.multiply(m, n)))
          }
          val ideals = (self.truth o possibleIdeals.toI) ?= isIdeal
          val idealMultiply = possibleIdeals.transpose(
            (ideals x monoid.carrier x monoid.carrier).biQuiver(self.omega) {
            case ((i, s), t) => ideals.include(i(monoid.multiply(s, t)))
            })
          new RightActionStar[TRUTH](monoid.rightAction(ideals, ideals.restrict(idealMultiply)))
        }
        override val truth: QUIVER[UNIT, TRUTH] =
          new RightActionQuiver[UNIT, TRUTH]_

        class RightActionStar[X <: ELEMENT](action: monoid.RightAction[X]) extends Star[X] {

        }

        class RightActionQuiver[S <: ELEMENT, T <: ELEMENT](
          val source: RightActionStar[S],
          val target: RightActionStar[T],
          val quiver: self.QUIVER[S, T]
          ) extends Quiver[S, T] {
        }

        class RightIdeal[IDEAL <: ELEMENT](ideal: self.STAR[IDEAL]) {

        }
      }
      */
}
