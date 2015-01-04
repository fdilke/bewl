package com.fdilke.bewl.actions

import com.fdilke.bewl.topos._
import com.fdilke.bewl.helper.Memoize

// Monoids and monoid actions defined as 'one-off' structures.
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

    lazy val rightRegularAction = 
      rightAction(carrier) { multiply(_, _) }

    lazy val rightActions = // : Topos with Wrappings[ELEMENT, RightAction, RightActionPrequiver] = // TODO: need type ?
      new RightMonoidActions

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

    type SCALAR = X // TODO: eliminate!!

    case class RightActionPrequiver[S <: ELEMENT, T <: ELEMENT](
      source: RightAction[S],
      target: RightAction[T],
      function: S => T
    )

    class RightMonoidActions extends Topos with 
      Wrappings[ELEMENT, RightAction, RightActionPrequiver] {
      override type ELEMENT = self.ELEMENT
      override type STAR[S <: ELEMENT] = RightActionStar[S]
      override type QUIVER[S <: ELEMENT, T <: ELEMENT] = RightActionQuiver[S, T]
      override type UNIT = self.UNIT
      override type TRUTH = SCALAR > self.TRUTH
      override val I = RightActionStar(self.I) { (i, m) => i }

      private object RightIdeals {
        private val possibleIdeals = carrier.power
        private val isIdeal = (carrier x carrier).forAll(possibleIdeals) {
          case (f, (m, n)) =>
            self.TruthObject.implies(f(m), f(multiply(m, n)))
        }
        private val ideals = possibleIdeals.toTrue ?= isIdeal
        
        private val idealMultiply = ideals.restrict(possibleIdeals.transpose(
          (ideals x carrier x carrier).biQuiver(self.omega) {
            case ((i, s), t) => ideals.inclusion(i)(multiply(s, t))
          }))

        val omega = RightActionStar[TRUTH](ideals)(
            self.BiQuiver(ideals x carrier, idealMultiply)(_,_)
          )

        def chi[S <: ELEMENT, T <: ELEMENT](monic: QUIVER[S, T]): QUIVER[T, TRUTH] = 
          new RightActionQuiver(monic.target, omega, 
            ideals.restrict(possibleIdeals.transpose(
              (monic.target.action.actionCarrier x carrier).biQuiver(self.omega) {
                (t, m) => monic.quiver.chi(monic.target.action.actionMultiply(t, m))
            })))
      }

      override val omega = RightIdeals.omega

      override val truth = 
        new RightActionQuiver(I, omega, carrier.power.transpose((self.I x carrier).biQuiver(self.omega) {
            case(x, m) => self.truth(x)
        }))

      class RightActionStar[A <: ELEMENT](
        private[RightMonoidActions] val action: RightAction[A]
        ) extends Star[A] { star =>
        override val toI: QUIVER[A, UNIT] = 
          new RightActionQuiver(this, I, action.actionCarrier.toI)

        override def sanityTest = {
          action.sanityTest
          action.actionCarrier.sanityTest
        }

        override def xUncached[B <: ELEMENT](that: STAR[B]): BIPRODUCT[A, B] = {
          val product = action.actionCarrier x that.action.actionCarrier
          new RightActionStar[A x B](rightAction(product){
            case ((x, y), s) => product.pair(
              action.actionMultiply(x, s), 
              that.action.actionMultiply(y, s)
              )
          }) with BiproductStar[A, B] {
            override val left: STAR[A] = star
            override val right: STAR[B] = that
            override def pair(l: A, r: B): x[A, B] = product.pair(l, r)
          }
        }

        override def `>Uncached`[T <: ELEMENT](that: STAR[T]): EXPONENTIAL[A, T] = {
          val pairs = carrier x action.actionCarrier
          val possibleMorphisms = pairs > that.action.actionCarrier
          val isMorphism = (pairs x carrier).forAll(possibleMorphisms) {
            case (f, ((s, x), t)) =>
              that.action.actionCarrier.diagonal(
                f(pairs.pair(multiply(s, t), this.action.actionMultiply(x, t))),
                that.action.actionMultiply(f(pairs.pair(s, x)), t)
              )
          }
          val morphisms = possibleMorphisms.toTrue ?= isMorphism
          val morphismMultiply = morphisms.restrict(possibleMorphisms.transpose(
            (morphisms x carrier x pairs).biQuiver(that.action.actionCarrier) {
              case ((f, s), (t, y)) => morphisms.inclusion(f)(
                pairs.pair(multiply(s, t), y)
            )}))
          new RightActionStar[(SCALAR x A) > T](rightAction(morphisms)(
              self.BiQuiver(morphisms x carrier, morphismMultiply).apply
            )) with ExponentialStar[SCALAR x A, T] { exponentialStar =>
            override val source = star.asInstanceOf[STAR[SCALAR x A]]
            override val target = that

            override def transpose[R <: ELEMENT](biQuiver: BiQuiver[R, SCALAR x A, T]) = {
              val lhs = biQuiver.product.left
              new RightActionQuiver(lhs, exponentialStar, morphisms.restrict(possibleMorphisms.transpose(
                (lhs.action.actionCarrier x pairs).biQuiver(that.action.actionCarrier) {
                  case (r, (t, x)) => biQuiver(
                    lhs.action.actionMultiply(r, t), 
                    x.asInstanceOf[SCALAR x A]
                  )})))}}.asInstanceOf[EXPONENTIAL[A, T]]
        }
        override def apply[T <: ELEMENT](target: STAR[T])(f: A => T): QUIVER[A, T] =
          new RightActionQuiver(this, target,
            action.actionCarrier(target.action.actionCarrier){ f })
      }

      object RightActionStar {
        def apply[A <: ELEMENT](actionCarrier: self.STAR[A])(actionMultiply: (A, SCALAR) => A) =
          new RightActionStar(rightAction(actionCarrier)(actionMultiply))
      }

      class RightActionQuiver[S <: ELEMENT, T <: ELEMENT](
        val source: RightActionStar[S],
        val target: RightActionStar[T],
        val quiver: self.QUIVER[S, T]
       ) extends Quiver[S, T] {
        override lazy val chi = RightIdeals.chi(this)

        override def \[U <: ELEMENT](monic: QUIVER[U, T]) = 
          new RightActionQuiver(source, monic.source, quiver \ monic.quiver)

        override def sanityTest = {
          quiver.sanityTest
          assert(source.action.isMorphism(target.action, quiver))
        }

        override def ?=(that: QUIVER[S, T]): EQUALIZER[S] = {
          val thunkedEqualizer = quiver ?= that.quiver
          new RightActionStar(rightAction(thunkedEqualizer)(source.action.actionMultiply(_, _))) with EqualizingStar[S] { equalizingStar =>
            override val equalizerTarget = source
            override val inclusion: QUIVER[S, S] = new RightActionQuiver(equalizingStar, source, thunkedEqualizer.inclusion)
            override def restrict[R <: ELEMENT](quiver: QUIVER[R, S]) =
              new RightActionQuiver(quiver.source, equalizingStar, 
                thunkedEqualizer.restrict(quiver.quiver))
          }
        }

        override def apply(s: S) = quiver(s)        

        override def o[R <: ELEMENT](that: QUIVER[R, S]): QUIVER[R, T] = 
          new RightActionQuiver(that.source, target, quiver o that.quiver)
      }

      override type WRAPPER[T <: ELEMENT] = T

      private val memoizedStarWrapper = {
        def wrap[T <: ELEMENT](input: RightAction[T]): STAR[T] =
          new RightActionStar(input)
        Memoize.generic.withLowerBound[RightAction, STAR, ELEMENT](wrap)
      }

      override def star[T <: ELEMENT](input: RightAction[T]): STAR[T] =
        memoizedStarWrapper(input)

      override def quiver[S <: ELEMENT, T <: ELEMENT](
        prequiver: RightActionPrequiver[S, T]
      ) = functionAsQuiver(
        star(prequiver.source), 
        star(prequiver.target), 
        prequiver.function
      )

      override def functionAsQuiver[S <: ELEMENT, T <: ELEMENT](source: STAR[S], target: STAR[T], f: S => T): QUIVER[S, T] =
        new RightActionQuiver(source, target, source.action.actionCarrier(target.action.actionCarrier) { f })

      override def bifunctionAsBiQuiver[L <: ELEMENT, R <: ELEMENT, T <: ELEMENT] (
        left: STAR[L],
        right: STAR[R],
        target: STAR[T]
      ) (
        bifunc: (L, R) => T
      ): BiQuiver[L, R, T] =
        (left x right).biQuiver(target) { bifunc }
    }    
  }
}


