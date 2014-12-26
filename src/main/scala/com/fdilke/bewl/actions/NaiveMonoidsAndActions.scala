package com.fdilke.bewl.actions

import com.fdilke.bewl.topos.{LogicalOperations, Topos, AlgebraicMachinery, BaseTopos}

// Monoids and monoid actions define as 'one-off' structures.
// TODO: once more general machinery is in place, update all monoid code and deprecate this

trait NaiveMonoidsAndActions { self: BaseTopos with AlgebraicMachinery with LogicalOperations =>
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
      new RightMonoidActions(this)

    case class RightAction[A <: ELEMENT] (
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

  private class RightMonoidActions[SCALAR <: ELEMENT](
    monoid: NaiveMonoid[SCALAR]
  ) extends Topos {
    import monoid.carrier

    override type ELEMENT = self.ELEMENT
    override type STAR[X <: ELEMENT] = RightActionStar[X]
    override type QUIVER[S <: ELEMENT, T <: ELEMENT] = RightActionQuiver[S, T]
    override type UNIT = self.UNIT
    override type TRUTH = SCALAR > self.TRUTH
    override val I =
      new RightActionStar[UNIT](monoid.rightAction(self.I,
        (self.I x carrier).biQuiver(self.I) {
          (i, m) => i
        }))

    override val omega = {
      val possibleIdeals = carrier.power
      val isIdeal = (carrier x carrier).forAll(possibleIdeals) {
        case (f, (m, n)) =>
          self.TruthObject.implies(f(m), f(monoid.multiply(m, n)))
      }
      val ideals = (self.truth o possibleIdeals.toI) ?= isIdeal
      val idealMultiply = ideals.restrict(possibleIdeals.transpose(
        (ideals x carrier x carrier).biQuiver(self.omega) {
          case ((i, s), t) => i(monoid.multiply(s, t)) // ideals.inclusion() somewhere?
        }))
      new RightActionStar[TRUTH](monoid.rightAction(ideals, self.BiQuiver(ideals x carrier, idealMultiply)))
    }

    override val truth = 
      new RightActionQuiver(I, omega, carrier.power.transpose((self.I x carrier).biQuiver(self.omega) {
          case(x, m) => self.truth(x)
      }))

    class RightActionStar[X <: ELEMENT](private[RightMonoidActions] val action: monoid.RightAction[X]) extends Star[X] {
      override val toI: QUIVER[X, UNIT] = 
        new RightActionQuiver(this, I, action.actionCarrier.toI)

      override def sanityTest = {}

      override def xUncached[T <: ELEMENT](that: STAR[T]): BIPRODUCT[X, T] = null

      override def `>Uncached`[T <: ELEMENT](that: STAR[T]): EXPONENTIAL[X, T] = null

      override def apply[T <: ELEMENT](target: STAR[T])(f: X => T): QUIVER[X, T] = null
    }

    class RightActionQuiver[S <: ELEMENT, T <: ELEMENT](
      val source: RightActionStar[S],
      val target: RightActionStar[T],
      val quiver: self.QUIVER[S, T]
     ) extends Quiver[S, T] {
      override val chi: QUIVER[T, TRUTH] = null

      override def \[U <: ELEMENT](monic: QUIVER[U, T]): QUIVER[S, U] = null

      override def sanityTest = {}

      override def ?=(that: QUIVER[S, T]): EQUALIZER[S] = null

      override def apply(s: S): T = null.asInstanceOf[T]

      override def o[R <: ELEMENT](that: QUIVER[R, S]): QUIVER[R, T] = null
    }
  }
}
