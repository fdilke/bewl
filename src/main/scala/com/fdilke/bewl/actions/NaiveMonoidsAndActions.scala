package com.fdilke.bewl.actions

import com.fdilke.bewl.topos.{Topos, AlgebraicMachinery, BaseTopos}

// Monoids and monoid actions define as 'one-off' structures.
// TODO: once more general machinery is in place, update all monoid code and deprecate this

trait NaiveMonoidsAndActions { self: BaseTopos with AlgebraicMachinery =>
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
      new Topos {
        override type ELEMENT = self.ELEMENT
        override type STAR = RightActionStar
        override type QUIVER = RightActionQuiver
        override type UNIT = self.UNIT
        override type TRUTH = SCALAR > self.TRUTH
        override val I =
          new RightActionStar[UNIT](monoid.rightAction(self.I,
            (self.I x monoid.carrier).biQuiver(self.I) {
              for(i <- self.I ; c <- monoid.carrier)
                yield i
            }))
        override val omega = {
          val possibleIdeals: EXPONENTIAL[SCALAR, TRUTH] = monoid.carrier > omega
          val ideals = (self.truth o possibleIdeals.toI) ?= possibleIdeals(omega) { r =>
            // TODO: fix: want "for all s,t: r(s) implies r(st)"
            (monoid.carrier x monoid.carrier).∀
            null.asInstanceOf[self.TRUTH]
          }
          new RightActionStar[TRUTH](monoid.rightAction(ideals,
            (ideals x monoid.carrier).biQuiver(ideals) {
              for(i <- ideals ; s <- monoid.carrier)
              yield i // TODO: yield x such that i(s(x)) is true
            }))
        }
        override val truth: QUIVER[UNIT, TRUTH] =
          new RightActionQuiver[UNIT, TRUTH]_

        class RightActionStar[X <: ELEMENT](action: monoid.RightAction[X]) extends Star[S] {

        }

        class RightActionQuiver[S <: ELEMENT, T <: ELEMENT](
          val source: RightActionStar[S],
          val target: RightActionStar[T],
          val quiver: QUIVER[S, T]
          ) extends Quiver[S, T] {
        }

        class RightIdeal[IDEAL <: ELEMENT](ideal: self.STAR[IDEAL]) {

        }
      }
*/
}
