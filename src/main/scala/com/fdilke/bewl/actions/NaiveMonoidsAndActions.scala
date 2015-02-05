package com.fdilke.bewl.actions

import com.fdilke.bewl.topos._
import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.helper.Duality

// Monoids and monoid actions defined as 'one-off' structures.
// TODO: once more general machinery is in place, update all monoid code and deprecate this

trait NaiveMonoidsAndActions { self: BaseTopos with AlgebraicMachinery with LogicalOperations =>

  case class ElementProxy0[A <: ELEMENT](element: A)
  // case class ElementProxy0(element: ELEMENT)

  trait UntypedElementProxy {
    type BASE <: ELEMENT
    val element: BASE
  }

  class ElementProxy[A <: ELEMENT](override val element: A) extends UntypedElementProxy {
    type BASE = A
  }

/*
  object ElementProxy {
    def pair[L <: UntypedElementProxy, R <: UntypedElementProxy](
      l: L, 
      r: R,
      productStar: BiproductStar[L#BASE, R#BASE]
    ) = 
    new (L, R)(l, r) with UntypedElementProxy {
      type BASE = L#BASE x R#BASE
      val element = productStar.pair(l.element, r.element) 
    }
  }
*/

  case class NaiveMonoid[M <: ELEMENT](carrier: STAR[M], unit: NullaryOp[M], multiply: BinaryOp[M]) {
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

    def rightAction[A <: ELEMENT](actionCarrier: STAR[A])(actionMultiply: (A, M) => A) =
      new RightAction[A](actionCarrier, actionMultiply)

    lazy val rightRegularAction = 
      rightAction(carrier) { multiply(_, _) }

    lazy val rightActions = new RightMonoidActions

    case class RightAction[A <: ELEMENT] (
      actionCarrier: STAR[A],
      actionMultiply: (A, M) => A
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

      type RIGHT_IDEAL = M > self.TRUTH
      override type TRUTH = RIGHT_IDEAL
      override val I = RightActionStar(self.I) { (i, m) => i }

      private object RightIdeals {
        private val possibleIdeals = carrier.power
        private val isIdeal = (carrier x carrier).forAll(possibleIdeals) {
          case (f, (m, n)) =>
            self.TruthObject.implies(f(m), f(multiply(m, n)))
        }
        private val ideals = possibleIdeals.toTrue ?= isIdeal
        
        def restrict[H <: self.ELEMENT](that: self.STAR[H])(bifunc: (H, M) => self.TRUTH): self.QUIVER[H, RIGHT_IDEAL] = 
          ideals.restrict(possibleIdeals.transpose(
            (that x carrier).biQuiver(self.omega)(bifunc)
          ))

        private val idealMultiply = restrict(ideals x carrier) {
            case ((i, s), t) => ideals.inclusion(i)(multiply(s, t))
          }

        val omega = RightActionStar[RIGHT_IDEAL](ideals)(
            self.BiQuiver[RIGHT_IDEAL, M, RIGHT_IDEAL](ideals x carrier, idealMultiply)(_, _)
          )
      }

      override lazy val omega = RightIdeals.omega

      override lazy val truth = 
        new RightActionQuiver(I, omega, carrier.power.transpose((self.I x carrier).biQuiver(self.omega) {
            case(x, m) => self.truth(x)
        }))

      class RightActionStar[A <: ELEMENT](
        private[RightMonoidActions] val action: RightAction[A]
        ) extends Star[A] { star =>
        override val toI: QUIVER[A, UNIT] = 
          new RightActionQuiver(this, I, action.actionCarrier.toI)

        override def sanityTest = {
          action.actionCarrier.sanityTest
          action.sanityTest
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

          new RightActionStar[(M x A) > T](rightAction(morphisms)(
              self.BiQuiver(morphisms x carrier, morphismMultiply).apply
            )) with ExponentialStar[M x A, T] { exponentialStar =>
            override val source = star.asInstanceOf[STAR[M x A]]
            override val target = that

            override def transpose[R <: ELEMENT](biQuiver: BiQuiver[R, M x A, T]) = {
              val lhs = biQuiver.product.left
              new RightActionQuiver(lhs, exponentialStar, morphisms.restrict(possibleMorphisms.transpose(
                (lhs.action.actionCarrier x pairs).biQuiver(that.action.actionCarrier) {
                  case (r, (t, x)) => biQuiver(
                    lhs.action.actionMultiply(r, t), 
                    x.asInstanceOf[M x A]
                  )})))}}.asInstanceOf[EXPONENTIAL[A, T]]
        }
        override def apply[T <: ELEMENT](target: STAR[T])(f: A => T): QUIVER[A, T] =
          new RightActionQuiver(this, target,
            action.actionCarrier(target.action.actionCarrier){ f })

        override def toString = "RightAction[" + action.actionCarrier + "]"
      }

      object RightActionStar {
        def apply[A <: ELEMENT](actionCarrier: self.STAR[A])(actionMultiply: (A, M) => A) =
          new RightActionStar(rightAction(actionCarrier)(actionMultiply))
      }

      class RightActionQuiver[S <: ELEMENT, T <: ELEMENT](
        val source: RightActionStar[S],
        val target: RightActionStar[T],
        val quiver: self.QUIVER[S, T]
       ) extends Quiver[S, T] {
        override lazy val chi = 
          new RightActionQuiver(target, omega, 
            RightIdeals.restrict(target.action.actionCarrier) {
                (t, m) => quiver.chi(target.action.actionMultiply(t, m)) 
            })

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

        override def toString = "RightActionQuiver[" + quiver + "]"

        override def equals(other: Any): Boolean = other match {
          case that: RightActionQuiver[S, T] =>
            source == that.source && target == that.target && quiver == that.quiver
          case _ => false
        }
        override def hashCode = 0
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

    class RightMonoidActionsInDraft extends Topos with 
      Wrappings[self.ELEMENT, RightAction, RightActionPrequiver] {
      override type ELEMENT = self.ElementProxy0[_]
      override type STAR[S <: ELEMENT] = RightActionStar[_, S]
      override type QUIVER[S <: ELEMENT, T <: ELEMENT] = RightActionQuiver[_, S, _, T]
      override type UNIT = self.ElementProxy0[self.UNIT]
      type RIGHT_IDEAL = self.>[M, self.TRUTH]
      override type TRUTH = self.ElementProxy0[RIGHT_IDEAL]
      override val I = RightActionStar(self.I) { (i, m) => i }

      private object RightIdeals {
        private val possibleIdeals = carrier.power
        private val isIdeal = (carrier x carrier).forAll(possibleIdeals) {
          case (f, (m, n)) => self.OmegaEnrichments(f(m)) > f(multiply(m, n))
        }
        private val ideals = possibleIdeals.toTrue ?= isIdeal

        def restrict[H <: self.ELEMENT](that: self.STAR[H])(bifunc: (H, M) => self.TRUTH): self.QUIVER[H, RIGHT_IDEAL] = 
          ideals.restrict(possibleIdeals.transpose(
            (that x carrier).biQuiver(self.omega)(bifunc)
          ))

        private val idealMultiply = restrict(ideals x carrier) {
            case ((i, s), t) => ideals.inclusion(i)(multiply(s, t))
          }
        val omega = RightActionStar[RIGHT_IDEAL](ideals)(
            self.BiQuiver[RIGHT_IDEAL, M, RIGHT_IDEAL](ideals x carrier, idealMultiply)(_, _)
          )
      }
      override lazy val omega = RightIdeals.omega

      override lazy val truth = 
        new RightActionQuiver(I, omega, carrier.power.transpose((self.I x carrier).biQuiver(self.omega) {
            case (x, m) => self.truth(x)
        }))

      class RightActionStar[A <: self.ELEMENT, E <: self.ElementProxy0[A]](
        private[RightMonoidActionsInDraft] val action: RightAction[A]
        ) extends Star[E] { star =>
        override val toI: QUIVER[E, UNIT] = 
          new RightActionQuiver(this, I, action.actionCarrier.toI)

        override def sanityTest = {
          action.actionCarrier.sanityTest
          action.sanityTest
        }

        // override def xUncached[B <: ELEMENT](that: STAR[B]): BIPRODUCT[E, B] = null
          // override def xUncached[B <: self.ELEMENT, F <: ELEMENT](that: RightActionStar[B, F]): BIPRODUCT[E, F] = {
        override def xUncached[F <: ELEMENT](that: STAR[F]): BIPRODUCT[E, F] = 
          that.preMultiplyUncached(this)

        private def preMultiplyUncached[Z <: self.ELEMENT, D <: self.ElementProxy0[Z]](pre: RightActionStar[Z, D]) : BIPRODUCT[D, E] = null
/*          
        private def preMultiplyUncached[Z <: self.ELEMENT, D <: self.ElementProxy0[Z]](pre: RightActionStar[Z, D]) : BIPRODUCT[D, E] = {
          val product = pre.action.actionCarrier x action.actionCarrier 
          val test1: (D, E) with self.ElementProxy0[self.x[Z, A]] = null.asInstanceOf[(D, E) with self.ElementProxy0[self.x[Z, A]]]
          val test2: D x E = test1
          val test3: D x E = null.asInstanceOf[D x E]
          val test4: (D, E) with self.ElementProxy0[self.x[Z, A]] = test3
          new RightActionStar[self.x[Z, A], (D, E) with self.ElementProxy0[self.x[Z, A]]] (
            rightAction(product){
              case ((z, a), m) => product.pair(
                  pre.action.actionMultiply(z, m),
                  action.actionMultiply(a, m) 
                )
              }) with BiproductStar[D, E] {
                override val left: STAR[D] = pre
                override val right: STAR[E] = star
                override def pair(l: D, r: E): x[D, E] = null.asInstanceOf[D x E]; // product.pair(l.element, r.element)
                // }.asInstanceOf[BIPRODUCT[E, F]] // TODO: fix cast?
              }
        }
*/
        override def `>Uncached`[T <: ELEMENT](that: STAR[T]): EXPONENTIAL[E, T] =  null
        override def apply[T <: ELEMENT](target: STAR[T])(f: E => T): QUIVER[E, T] = null

        override def toString = "RightAction[" + action.actionCarrier + "]"
      }

      object RightActionStar {
        def apply[A <: self.ELEMENT](actionCarrier: self.STAR[A])(actionMultiply: (A, M) => A) =
          new RightActionStar[A, self.ElementProxy0[A]](rightAction(actionCarrier)(actionMultiply))
      }

      class RightActionQuiver[S <: self.ELEMENT, E <: self.ElementProxy0[S], T <: self.ELEMENT, F <: self.ElementProxy0[T]](
        val source: RightActionStar[S, E],
        val target: RightActionStar[T, F],
        val quiver: self.QUIVER[S, T]
       ) extends Quiver[E, F] {
        override lazy val chi: QUIVER[F, TRUTH] = 
          new RightActionQuiver(target, omega, 
            RightIdeals.restrict(target.action.actionCarrier) {
                (t, m) => quiver.chi(target.action.actionMultiply(t, m)) 
            })

        override def \[U <: ELEMENT](monic: QUIVER[U, F]) = null

        override def sanityTest = {
          quiver.sanityTest
          assert(source.action.isMorphism(target.action, quiver))
        }

        override def ?=(that: QUIVER[E, F]): EQUALIZER[E] = null

        override def apply(e: E) = null.asInstanceOf[F]

        override def o[R <: ELEMENT](that: QUIVER[R, E]): QUIVER[R, F] = null

        override def toString = "RightActionQuiver[" + quiver + "]"

        override def equals(other: Any): Boolean = other match {
          case that: RightActionQuiver[S, E, T, F] =>
            source == that.source && target == that.target && quiver == that.quiver
          case _ => false
        }
        override def hashCode = 0
      }

      override type WRAPPER[T <: self.ELEMENT] = self.ElementProxy0[T]

      private val memoizedStarWrapper = {
        type STAR_WRAPPER[T <: self.ELEMENT] = STAR[WRAPPER[T]]
        def wrap[T <: self.ELEMENT](input: RightAction[T]): STAR_WRAPPER[T] =
          new RightActionStar[T, WRAPPER[T]](input)
        Memoize.generic.withLowerBound[RightAction, STAR_WRAPPER, self.ELEMENT](wrap)
      }

      override def star[T <: self.ELEMENT](input: RightAction[T]): STAR[WRAPPER[T]] =
        memoizedStarWrapper(input)

      override def quiver[S <: self.ELEMENT, T <: self.ELEMENT](
        prequiver: RightActionPrequiver[S, T]
      ) = functionAsQuiver(
        star(prequiver.source), 
        star(prequiver.target), 
        prequiver.function
      )

      override def functionAsQuiver[S <: self.ELEMENT, T <: self.ELEMENT](
        source: STAR[WRAPPER[S]], 
        target: STAR[WRAPPER[T]], 
        f: S => T
      ): QUIVER[WRAPPER[S], WRAPPER[T]] = {
        val src = source.asInstanceOf[RightActionStar[S, WRAPPER[S]]]
        val tgt = target.asInstanceOf[RightActionStar[T, WRAPPER[T]]]

        new RightActionQuiver[S, WRAPPER[S], T, WRAPPER[T]](
          src, 
          tgt, 
          src.action.actionCarrier(tgt.action.actionCarrier) { f })
      }

      override def bifunctionAsBiQuiver[
        L <: self.ELEMENT, 
        R <: self.ELEMENT, 
        T <: self.ELEMENT
      ] (
        left: STAR[WRAPPER[L]],
        right: STAR[WRAPPER[R]],
        target: STAR[WRAPPER[T]]
      ) (
        bifunc: (L, R) => T
      ): BiQuiver[WRAPPER[L], WRAPPER[R], WRAPPER[T]] = {
        val l =   left.asInstanceOf[RightActionStar[L, WRAPPER[L]]]
        val r =   left.asInstanceOf[RightActionStar[R, WRAPPER[R]]]
        val t = target.asInstanceOf[RightActionStar[T, WRAPPER[T]]]
        (l x r).biQuiver(t) { (a, b) =>
          self.ElementProxy0(bifunc(a.element, b.element))
        }
      }
    }

    class RightMonoidActionsInDraft2 extends Topos with 
      Wrappings[self.ELEMENT, RightAction, RightActionPrequiver] {
      override type ELEMENT = self.UntypedElementProxy
      override type STAR[E <: ELEMENT] = RightActionStarFacade[E]
      override type QUIVER[E <: ELEMENT, F <: ELEMENT] = RightActionQuiverFacade[E, F]
      override type UNIT = self.ElementProxy[self.UNIT]
      type RIGHT_IDEAL = self.>[M, self.TRUTH]
      override type TRUTH = self.ElementProxy[RIGHT_IDEAL]
      override val I = RightActionStar(self.I) { (i, m) => i }

      private object RightIdeals {
        private val possibleIdeals = carrier.power
        private val isIdeal = (carrier x carrier).forAll(possibleIdeals) {
          case (f, (m, n)) => self.OmegaEnrichments(f(m)) > f(multiply(m, n))
        }
        private val ideals = possibleIdeals.toTrue ?= isIdeal

        def restrict[H <: self.ELEMENT](that: self.STAR[H])(bifunc: (H, M) => self.TRUTH): self.QUIVER[H, RIGHT_IDEAL] = 
          ideals.restrict(possibleIdeals.transpose(
            (that x carrier).biQuiver(self.omega)(bifunc)
          ))

        private val idealMultiply = restrict(ideals x carrier) {
            case ((i, s), t) => ideals.inclusion(i)(multiply(s, t))
          }
        val omega = RightActionStar[RIGHT_IDEAL](ideals)(
            self.BiQuiver[RIGHT_IDEAL, M, RIGHT_IDEAL](ideals x carrier, idealMultiply)(_, _)
          )
      }
      override lazy val omega = RightIdeals.omega
      override lazy val truth = 
        new RightActionQuiver(I, omega, carrier.power.transpose((self.I x carrier).biQuiver(self.omega) {
            case (x, m) => self.truth(x)
        }))

      trait RightActionStarFacade[E <: self.UntypedElementProxy] extends Star[E] { facade => 
        private[RightMonoidActionsInDraft2] def preMultiplyUncached[Z <: self.ELEMENT, D <: ELEMENT](
          pre: RightActionStar[Z] with RightActionStarFacade[D]
        ) : BIPRODUCT[D, E]
        private[RightMonoidActionsInDraft2] def preExponentiateUncached[Z <: self.ELEMENT, D <: ELEMENT](
          pre: RightActionStar[Z] with RightActionStarFacade[D]
        ) : EXPONENTIAL[D, E]
      }

      private class LaxRightActionStarFacade[E <: self.UntypedElementProxy, F <: self.UntypedElementProxy](
          delegate: RightActionStarFacade[E],
          Δ: Duality[E, F]
        ) extends RightActionStarFacade[F] {
        lazy val toI: QUIVER[F, UNIT] =
          new LeftLaxRightActionQuiverFacade(delegate.toI, Δ, this)
        def xUncached[T <: ELEMENT](that: STAR[T]): BIPRODUCT[F, T] =
          new LeftLaxBiproduct(delegate.xUncached(that), Δ, this)
        def `>Uncached`[T <: ELEMENT](that: STAR[T]): EXPONENTIAL[F, T] =
          new LeftLaxExponentialStar(delegate.`>Uncached`(that), Δ, this)
        def apply[T <: ELEMENT](target: STAR[T])(f: F => T) : QUIVER[F, T] = 
          new LeftLaxRightActionQuiverFacade(delegate(target)(f compose (Δ./)), Δ, this)
        def sanityTest = delegate.sanityTest

        override private[RightMonoidActionsInDraft2] def preMultiplyUncached[Z <: self.ELEMENT, D <: ELEMENT](
          pre: RightActionStar[Z] with RightActionStarFacade[D]
        ) : BIPRODUCT[D, F] = 
          new RightLaxBiproduct(delegate.preMultiplyUncached[Z, D](pre), Δ, this)

        private[RightMonoidActionsInDraft2] def preExponentiateUncached[Z <: self.ELEMENT, D <: ELEMENT](
          pre: RightActionStar[Z] with RightActionStarFacade[D]
        ) : EXPONENTIAL[D, F] =
          new RightLaxExponentialStar(delegate.preExponentiateUncached[Z, D](pre), Δ, this)
      }

      private class RightLaxBiproduct[
        D <: self.UntypedElementProxy,
        E <: self.UntypedElementProxy, 
        F <: self.UntypedElementProxy
      ] (
        delegate: BIPRODUCT[D, E],
        Δ: Duality[E, F],
        laxStar: STAR[F]
      ) extends LaxRightActionStarFacade[D x E, D x F](delegate,
        new Duality[D x E, D x F] (
          dxe => rightLaxPair(dxe._1, Δ / dxe._2, delegate, Δ),
          dxf => delegate.pair(dxf._1, Δ \ dxf._2)
        )
      ) with BiproductStar[D, F] {
        val left: STAR[D] = delegate.left
        val right: STAR[F] = laxStar
        def pair(d: D, f: F): D x F = rightLaxPair(d, f, delegate, Δ)
      }

      private def laxPair[
        E <: self.UntypedElementProxy, 
        F <: self.UntypedElementProxy,
        G <: self.UntypedElementProxy
        ] (delegate: E, f: F, g: G): F x G =
          new (F, G)(f, g) with self.UntypedElementProxy {
            override type BASE = delegate.BASE
            override val element = delegate.element
          }

      private def leftLaxPair[
        E <: self.UntypedElementProxy, 
        F <: self.UntypedElementProxy,
        G <: self.UntypedElementProxy
      ] (f: F, g: G, biproduct: BIPRODUCT[E, G], Δ: Duality[E, F]) : F x G = 
          laxPair(biproduct.pair(Δ \ f, g), f, g)

      private def rightLaxPair[
        D <: self.UntypedElementProxy,
        E <: self.UntypedElementProxy, 
        F <: self.UntypedElementProxy
      ] (d: D, f: F, biproduct: BIPRODUCT[D, E], Δ: Duality[E, F]) : D x F = 
          laxPair(biproduct.pair(d, Δ \ f), d, f)

      private class LeftLaxBiproduct[
        E <: self.UntypedElementProxy, 
        F <: self.UntypedElementProxy,
        G <: self.UntypedElementProxy
      ] (
        delegate: BIPRODUCT[E, G],
        Δ: Duality[E, F],
        laxStar: STAR[F]
      ) extends LaxRightActionStarFacade[E x G, F x G](delegate, 
        new Duality[E x G, F x G](
          exg => leftLaxPair(Δ / (exg._1), exg._2, delegate, Δ),
          fxg => delegate.pair(Δ \ (fxg._1), fxg._2)
        )
      ) with BiproductStar[F, G] {
        val left: STAR[F] = laxStar
        val right: STAR[G] = delegate.right
        def pair(f: F, g: G): F x G = leftLaxPair(f, g, delegate, Δ)
      }

      private def leftLaxExponential[
        E <: self.UntypedElementProxy, 
        F <: self.UntypedElementProxy,
        G <: self.UntypedElementProxy
      ] (e2g: E > G, Δ: Duality[E, F]) : F > G = 
          new (F => G) with self.UntypedElementProxy {
            override def apply(f: F): G = e2g(Δ \ f)
            override type BASE = e2g.BASE
            override val element = e2g.element
          }

      private def rightLaxExponential[
        D <: self.UntypedElementProxy, 
        E <: self.UntypedElementProxy,
        F <: self.UntypedElementProxy
      ] (d2e: D > E, Δ: Duality[E, F]) : D > F = 
          new (D => F) with self.UntypedElementProxy {
            override def apply(d: D): F = Δ / d2e(d)
            override type BASE = d2e.BASE
            override val element = d2e.element
          }

      private class LeftLaxExponentialStar[
        E <: self.UntypedElementProxy, 
        F <: self.UntypedElementProxy,
        G <: self.UntypedElementProxy
      ] (
        delegate: EXPONENTIAL[E, G],
        Δ: Duality[E, F],
        laxStar: STAR[F]
      ) extends LaxRightActionStarFacade[E > G, F > G](delegate,
        new Duality[E > G, F > G](
          e2g => leftLaxExponential(e2g, Δ),
          f2g => leftLaxExponential(f2g, ~Δ)
        )
      ) with ExponentialStar[F, G] { laxExponential =>
        val source: STAR[F] = laxStar
        val target: STAR[G] = delegate.target
        def transpose[R <: ELEMENT](biQuiver: BiQuiver[R, F, G]): QUIVER[R, F > G] = {
          val rStar: STAR[R] = biQuiver.product.left
          rStar(laxExponential) { r => 
            leftLaxExponential(delegate.transpose(
              (rStar x delegate.source).biQuiver(delegate.target) {
                (r, e) => biQuiver(r, Δ / e)
              }
            )(r), Δ)
          }
        }
      }

      private class RightLaxExponentialStar[
        D <: self.UntypedElementProxy, 
        E <: self.UntypedElementProxy,
        F <: self.UntypedElementProxy
      ] (
        delegate: EXPONENTIAL[D, E],
        Δ: Duality[E, F],
        laxStar: STAR[F]
      ) extends LaxRightActionStarFacade[D > E, D > F](delegate,
        new Duality[D > E, D > F](
          d2e => rightLaxExponential(d2e, Δ),
          d2f => rightLaxExponential(d2f, ~Δ)
        )
      ) with ExponentialStar[D, F] { laxExponential =>
        val source: STAR[D] = delegate.source
        val target: STAR[F] = laxStar
        def transpose[R <: ELEMENT](biQuiver: BiQuiver[R, D, F]): QUIVER[R, D > F] = {
          val rStar: STAR[R] = biQuiver.product.left
          rStar(laxExponential) { r =>
            rightLaxExponential(delegate.transpose(
              (rStar x delegate.source).biQuiver(delegate.target) {
                (r, d) => Δ \ biQuiver(r, d)
              }
            )(r), Δ)
          }
        }
      }

      class RightActionStar[A <: self.ELEMENT](private[RightMonoidActionsInDraft2] val action: RightAction[A]) extends
        RightActionStarFacade[self.ElementProxy[A]] { star =>
        private type E = self.ElementProxy[A]

        override val toI: QUIVER[E, UNIT] = 
          new RightActionQuiver(this, I, action.actionCarrier.toI)

        override def sanityTest = {
          action.actionCarrier.sanityTest
          action.sanityTest
        }

        override def xUncached[F <: ELEMENT](that: STAR[F]): BIPRODUCT[E, F] = 
          that.preMultiplyUncached[A, E](this)

        override private[RightMonoidActionsInDraft2] def 
          preMultiplyUncached[Z <: self.ELEMENT, D <: ELEMENT](
          pre: RightActionStar[Z] with RightActionStarFacade[D]
        ) : BIPRODUCT[D, E] = {
          val product = pre.action.actionCarrier x action.actionCarrier 
          val innerStar = new RightActionStar[self.x[Z, A]] (
            rightAction(product){
              case ((z, a), m) => product.pair(
                  pre.action.actionMultiply(z, m),
                  action.actionMultiply(a, m) 
                )
              })
          val innerBiproduct: self.BIPRODUCT[Z, A] = pre.action.actionCarrier x action.actionCarrier
          val Δ = new Duality[self.ElementProxy[self.x[Z, A]], D x E](
            zxa => zxa.element match { case (z, a) =>
                new (D, E)(
                  new self.ElementProxy[Z](z).asInstanceOf[D], // TODO: fix cast
                  new self.ElementProxy[A](a)
                ) with self.UntypedElementProxy {
                  override type BASE = self.x[Z, A]
                  override val element = zxa.element
                }
              },
              dxe => new self.ElementProxy(dxe.element.asInstanceOf[self.x[Z, A]])
            )
          new LaxRightActionStarFacade(innerStar, Δ) with BiproductStar[D, E] {
                override val left: STAR[D] = pre
                override val right: STAR[E] = star
                override def pair(d: D, e: E): x[D, E] = { 
                  val z: Z = d.element.asInstanceOf[Z] // TODO: fix cast
                  val a: A = e.element
                  new (D, E)(d, e) with self.UntypedElementProxy {
                    override type BASE = self.x[Z, A]
                    override val element = innerBiproduct.pair(z, a)
                  }
                }
              }
        }

        override def `>Uncached`[T <: ELEMENT](that: STAR[T]): EXPONENTIAL[E, T] =  
          that.preExponentiateUncached[A, E](this)

        override private[RightMonoidActionsInDraft2] def 
          preExponentiateUncached[Z <: self.ELEMENT, D <: ELEMENT](
          pre: RightActionStar[Z] with RightActionStarFacade[D]
        ) : EXPONENTIAL[D, E] = {
            val pairs = carrier x pre.action.actionCarrier
            val possibleMorphisms = pairs > action.actionCarrier
            val isMorphism = (pairs x carrier).forAll(possibleMorphisms) {
              case (f, ((s, x), t)) =>
                action.actionCarrier.diagonal(
                  f(pairs.pair(multiply(s, t), pre.action.actionMultiply(x, t))),
                  action.actionMultiply(f(pairs.pair(s, x)), t)
                )
            }
            val morphisms = possibleMorphisms.toTrue ?= isMorphism
            val morphismMultiply = morphisms.restrict(possibleMorphisms.transpose(
              (morphisms x carrier x pairs).biQuiver(action.actionCarrier) {
                case ((f, s), (t, y)) => morphisms.inclusion(f)(
                  pairs.pair(multiply(s, t), y)
              )}))
      // to make this work, quivers need to be between star facades
            type P = self.>[self.x[M, Z],A]
            type Q = self.ElementProxy[P]

            val innerExpStar: STAR[Q] = new RightActionStar[P](rightAction(morphisms)(
              self.BiQuiver(morphisms x carrier, morphismMultiply).apply
            )) 
            val Δ = new Duality[Q, D > E](
              q => new (D => E) with self.UntypedElementProxy {
                override def apply(d: D):E = {
                  val z = d.element.asInstanceOf[Z] // TODO: fix cast
                  val p: P = q.element
                  val unitM: M = unit(pre.action.actionCarrier.toI(z))
                  val a: A = p(pairs.pair(unitM, z))
                  new self.ElementProxy(a)
                }
                override type BASE = q.BASE
                override val element = q.element
              },
              d2e => {
                val p = d2e.element.asInstanceOf[P] // TODO: fix cast?
                new self.ElementProxy(p)
              }
            )
            new LaxRightActionStarFacade(innerExpStar, Δ) with ExponentialStar[D, E] { exponentialStar =>
              val source: STAR[D] = pre
              val target: STAR[E] = star
              def transpose[R <: ELEMENT](biQuiver: BiQuiver[R, D, E]): QUIVER[R, D > E] = {
                // val lhs: STAR[R] = biQuiver.product.left
                // val innerQ = morphisms.restrict(possibleMorphisms.transpose(
                //   (lhs.action.actionCarrier x pairs).biQuiver(action.actionCarrier) {
                //     case (r, (t, x)) => biQuiver(
                //       lhs.action.actionMultiply(r, t), 
                //       x.asInstanceOf[M x A]
                //     )
                //     }))
                // lhs(exponentialStar) {
                //   ... something from innerQ
                // }
                ???
              }
              // {
              //   val lhs: STAR[R] = biQuiver.product.left
              //   new RightActionQuiver(lhs, exponentialStar, morphisms.restrict(possibleMorphisms.transpose(
              //     (lhs.action.actionCarrier x pairs).biQuiver(action.actionCarrier) {
              //       case (r, (t, x)) => biQuiver(
              //         lhs.action.actionMultiply(r, t), 
              //         x.asInstanceOf[M x A]
              //       )
              //       })))
              // }
            }
          }

        override def apply[T <: ELEMENT](target: STAR[T])(f: E => T): QUIVER[E, T] = null

        override def toString = "RightActionStar[" + action.actionCarrier + "]"
      }

      object RightActionStar {
        def apply[A <: self.ELEMENT](actionCarrier: self.STAR[A])(actionMultiply: (A, M) => A) =
          new RightActionStar[A](rightAction(actionCarrier)(actionMultiply))
      }

      trait RightActionQuiverFacade[E <: self.UntypedElementProxy, F <: self.UntypedElementProxy] extends
        Quiver[E, F] { facade =>
      }

      // TODO: do we need this? Can a quiver just be between StarFacades?
      class LeftLaxRightActionQuiverFacade[
        E <: self.UntypedElementProxy, 
        F <: self.UntypedElementProxy,
        G <: self.UntypedElementProxy
      ](
          delegate: RightActionQuiverFacade[E, G],
          Δ: Duality[E, F],
          val source: RightActionStarFacade[F]
        ) extends RightActionQuiverFacade[F, G] {
        val target: STAR[G] = delegate.target
        lazy val chi: QUIVER[G, TRUTH] = delegate.chi

        def apply(f: F): G = delegate(Δ \ f)
        def ?=(that: QUIVER[F, G]): EQUALIZER[F] =
          null
        def o[R <: ELEMENT](that: QUIVER[R, F]) : QUIVER[R, G] =
          that.source(delegate.target) { r =>
            delegate(Δ \ (that(r)))
          }
        def \[U <: ELEMENT](monic: QUIVER[U, G]) : QUIVER[F, U] =
          source(monic.source) { f => 
            (delegate \ monic)(Δ \ f)
          }
        def sanityTest = delegate.sanityTest
      }

      class RightActionQuiver[A <: self.ELEMENT, B <: self.ELEMENT](
        val source: RightActionStar[A],
        val target: RightActionStar[B],
        val quiver: self.QUIVER[A, B]
      ) extends RightActionQuiverFacade[self.ElementProxy[A], self.ElementProxy[B]] {
        private type E = self.ElementProxy[A]
        private type F = self.ElementProxy[B]

        override lazy val chi: QUIVER[F, TRUTH] = null

        override def \[U <: ELEMENT](monic: QUIVER[U, F]) = null

        override def sanityTest = {
          quiver.sanityTest
          assert(source.action.isMorphism(target.action, quiver))
        }

        override def ?=(that: QUIVER[E, F]): EQUALIZER[E] = null

        override def apply(e: E) = null.asInstanceOf[F]

        override def o[R <: ELEMENT](that: QUIVER[R, E]): QUIVER[R, F] = null

        override def toString = "RightActionQuiver[" + quiver + "]"

        override def equals(other: Any): Boolean = false
        override def hashCode = 0
      }

      override type WRAPPER[T <: self.ELEMENT] = self.ElementProxy[T]

      private val memoizedStarWrapper = {
        type STAR_WRAPPER[T <: self.ELEMENT] = STAR[WRAPPER[T]]
        def wrap[T <: self.ELEMENT](input: RightAction[T]): STAR_WRAPPER[T] =
          null // new RightActionStar[T, WRAPPER[T]](input)
        Memoize.generic.withLowerBound[RightAction, STAR_WRAPPER, self.ELEMENT](wrap)
      }

      override def star[T <: self.ELEMENT](input: RightAction[T]): STAR[WRAPPER[T]] =
        null // memoizedStarWrapper(input)

      override def quiver[S <: self.ELEMENT, T <: self.ELEMENT](
        prequiver: RightActionPrequiver[S, T]
      ) = functionAsQuiver(
        star(prequiver.source), 
        star(prequiver.target), 
        prequiver.function
      )

      override def functionAsQuiver[S <: self.ELEMENT, T <: self.ELEMENT](
        source: STAR[WRAPPER[S]], 
        target: STAR[WRAPPER[T]], 
        f: S => T
      ): QUIVER[WRAPPER[S], WRAPPER[T]] = {
        null
        // val src = source.asInstanceOf[RightActionStar[S, WRAPPER[S]]]
        // val tgt = target.asInstanceOf[RightActionStar[T, WRAPPER[T]]]
        // new RightActionQuiver[S, WRAPPER[S], T, WRAPPER[T]](
        //   src, 
        //   tgt, 
        //   src.action.actionCarrier(tgt.action.actionCarrier) { f })
      }

      override def bifunctionAsBiQuiver[
        L <: self.ELEMENT, 
        R <: self.ELEMENT, 
        T <: self.ELEMENT
      ] (
        left: STAR[WRAPPER[L]],
        right: STAR[WRAPPER[R]],
        target: STAR[WRAPPER[T]]
      ) (
        bifunc: (L, R) => T
      ): BiQuiver[WRAPPER[L], WRAPPER[R], WRAPPER[T]] = {
        null
        // val l =   left.asInstanceOf[RightActionStar[L, WRAPPER[L]]]
        // val r =   left.asInstanceOf[RightActionStar[R, WRAPPER[R]]]
        // val t = target.asInstanceOf[RightActionStar[T, WRAPPER[T]]]
        // (l x r).biQuiver(t) { (a, b) =>
        //   self.ElementProxy0(bifunc(a.element, b.element))
        // }
      }
    }
  }
}

