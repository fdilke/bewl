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

    def rightAction[A <: ELEMENT](actionCarrier: STAR[A])(actionMultiply: (A, X) => A) =
      new RightAction[A](actionCarrier, actionMultiply)

    lazy val rightActions : Topos =
      new RightMonoidActions(this)

    case class RightAction[A <: ELEMENT] (
      actionCarrier: STAR[A],
      actionMultiply: (A, X) => A
    ) {
      def isMorphism[B <: ELEMENT](that: RightAction[B], quiver: QUIVER[A, B]) = 
        (quiver.source == this.actionCarrier) &&
        (quiver.target == that.actionCarrier) && (
          carrier.forAll(actionCarrier) { (x, m) =>
            that.actionCarrier.diagonal(
              quiver(this.actionMultiply(x, m)),
              that.actionMultiply(quiver(x), m)
            )
          } == actionCarrier.toTrue
        )
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
    override val I = RightActionStar(self.I) { (i, m) => i }

    override val omega = {
      val possibleIdeals = carrier.power
      val isIdeal = (carrier x carrier).forAll(possibleIdeals) {
        case (f, (m, n)) =>
          self.TruthObject.implies(f(m), f(monoid.multiply(m, n)))
      }
      val ideals = possibleIdeals.toTrue ?= isIdeal
      val idealMultiply = ideals.restrict(possibleIdeals.transpose(
        (ideals x carrier x carrier).biQuiver(self.omega) {
          case ((i, s), t) => ideals.inclusion(i)(monoid.multiply(s, t))
        }))
      RightActionStar[TRUTH](ideals)(
          self.BiQuiver(ideals x carrier, idealMultiply).apply
        )
    }

    override val truth = 
      new RightActionQuiver(I, omega, carrier.power.transpose((self.I x carrier).biQuiver(self.omega) {
          case(x, m) => self.truth(x)
      }))

    class RightActionStar[X <: ELEMENT](
      private[RightMonoidActions] val action: monoid.RightAction[X]
      ) extends Star[X] { star =>
      override val toI: QUIVER[X, UNIT] = 
        new RightActionQuiver(this, I, action.actionCarrier.toI)

      override def sanityTest = {
        action.sanityTest
        action.actionCarrier.sanityTest
      }

      override def xUncached[Y <: ELEMENT](that: STAR[Y]): BIPRODUCT[X, Y] = {
        val product = action.actionCarrier x that.action.actionCarrier
        new RightActionStar[X x Y](monoid.rightAction(product){
          case ((x, y), s) => product.pair(
            action.actionMultiply(x, s), 
            that.action.actionMultiply(y, s)
            )
        }) with BiproductStar[X, Y] {
          override val left: STAR[X] = star
          override val right: STAR[Y] = that
          override def pair(l: X, r: Y): x[X, Y] = product.pair(l, r)
        }
      }

      override def `>Uncached`[T <: ELEMENT](that: STAR[T]): EXPONENTIAL[X, T] = {
        val pairs = carrier x action.actionCarrier
        val possibleMorphisms = pairs > that.action.actionCarrier
        val isMorphism = (pairs x carrier).forAll(possibleMorphisms) {
          case (f, ((s, x), t)) =>
            that.action.actionCarrier.diagonal(
              f(pairs.pair(monoid.multiply(s, t), this.action.actionMultiply(x, t))),
              that.action.actionMultiply(f(pairs.pair(s, x)), t)
            )
        }
        val morphisms = possibleMorphisms.toTrue ?= isMorphism
        val morphismMultiply = morphisms.restrict(possibleMorphisms.transpose(
          (morphisms x carrier x pairs).biQuiver(that.action.actionCarrier) {
            case ((f, s), (t, y)) => morphisms.inclusion(f)(
              pairs.pair(monoid.multiply(s, t), y)
          )}))
        new RightActionStar[(SCALAR x X) > T](monoid.rightAction(morphisms)(
            self.BiQuiver(morphisms x carrier, morphismMultiply).apply
          )) with ExponentialStar[SCALAR x X, T] { exponentialStar =>
          override val source = star.asInstanceOf[STAR[SCALAR x X]]
          override val target = that

          override def transpose[R <: ELEMENT](biQuiver: BiQuiver[R, SCALAR x X, T]) = {
            val lhs = biQuiver.product.left
            new RightActionQuiver(lhs, exponentialStar, morphisms.restrict(possibleMorphisms.transpose(
              (lhs.action.actionCarrier x pairs).biQuiver(that.action.actionCarrier) {
                case (r, (t, x)) => biQuiver(
                  lhs.action.actionMultiply(r, t), 
                  x.asInstanceOf[SCALAR x X]
                )})))}}.asInstanceOf[EXPONENTIAL[X, T]]
      }
      override def apply[T <: ELEMENT](target: STAR[T])(f: X => T): QUIVER[X, T] =
        new RightActionQuiver(this, target,
          action.actionCarrier(target.action.actionCarrier){ f })
    }

    object RightActionStar {
      def apply[A <: ELEMENT](actionCarrier: self.STAR[A])(actionMultiply: (A, SCALAR) => A) =
        new RightActionStar(monoid.rightAction(actionCarrier)(actionMultiply))
    }

    class RightActionQuiver[S <: ELEMENT, T <: ELEMENT](
      val source: RightActionStar[S],
      val target: RightActionStar[T],
      val quiver: self.QUIVER[S, T]
     ) extends Quiver[S, T] {
      override val chi: QUIVER[T, TRUTH] = null

      override def \[U <: ELEMENT](monic: QUIVER[U, T]): QUIVER[S, U] = null

      override def sanityTest = {
        quiver.sanityTest
        assert(source.action.isMorphism(target.action, quiver))
      }

      override def ?=(that: QUIVER[S, T]): EQUALIZER[S] = null

      override def apply(s: S): T = null.asInstanceOf[T]

      override def o[R <: ELEMENT](that: QUIVER[R, S]): QUIVER[R, T] = null
    }
  }
}
