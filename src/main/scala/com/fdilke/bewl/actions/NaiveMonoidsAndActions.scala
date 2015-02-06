package com.fdilke.bewl.actions

import com.fdilke.bewl.topos._
import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.helper.↔

// Monoids and monoid actions defined as 'one-off' structures.
// TODO: once more general machinery is in place, update all monoid code and deprecate this

trait NaiveMonoidsAndActions { Ɛ: BaseTopos with AlgebraicMachinery with LogicalOperations =>

  case class ElementProxy0[A <: ELEMENT](element: A)

  trait ElementWrapper[A <: ELEMENT] { wrapper =>
    val element: A

    def apply[F, G](f: F, g: G): (F, G) with ElementWrapper[A] =
      new (F, G)(f, g) with ElementWrapper[A] {
        override val element = wrapper.element
      }

    def apply[F, G](f2g: F => G): (F => G) with ElementWrapper[A] =
      new (F => G) with ElementWrapper[A] {
        def apply(f: F): G = f2g(f)
        override val element = wrapper.element
      }
  }

  object ElementWrapper {
    def apply[A <: ELEMENT](a: A) =
      new ElementWrapper[A] {
        override val element = a
      }

    def ↔[A <: ELEMENT] =
      new ↔[A, ElementWrapper[A]](
        a => ElementWrapper(a),
        aa => aa.element
      )
  }

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
      override type ELEMENT = Ɛ.ELEMENT
      override type STAR[S <: ELEMENT] = RightActionStar[S]
      override type QUIVER[S <: ELEMENT, T <: ELEMENT] = RightActionQuiver[S, T]
      override type UNIT = Ɛ.UNIT

      type RIGHT_IDEAL = M > Ɛ.TRUTH
      override type TRUTH = RIGHT_IDEAL
      override val I = RightActionStar(Ɛ.I) { (i, m) => i }

      private object RightIdeals {
        private val possibleIdeals = carrier.power
        private val isIdeal = (carrier x carrier).forAll(possibleIdeals) {
          case (f, (m, n)) =>
            Ɛ.TruthObject.implies(f(m), f(multiply(m, n)))
        }
        private val ideals = possibleIdeals.toTrue ?= isIdeal
        
        def restrict[H <: Ɛ.ELEMENT](that: Ɛ.STAR[H])(bifunc: (H, M) => Ɛ.TRUTH): Ɛ.QUIVER[H, RIGHT_IDEAL] = 
          ideals.restrict(possibleIdeals.transpose(
            (that x carrier).biQuiver(Ɛ.omega)(bifunc)
          ))

        private val idealMultiply = restrict(ideals x carrier) {
            case ((i, s), t) => ideals.inclusion(i)(multiply(s, t))
          }

        val omega = RightActionStar[RIGHT_IDEAL](ideals)(
            Ɛ.BiQuiver[RIGHT_IDEAL, M, RIGHT_IDEAL](ideals x carrier, idealMultiply)(_, _)
          )
      }

      override lazy val omega = RightIdeals.omega

      override lazy val truth = 
        new RightActionQuiver(I, omega, carrier.power.transpose((Ɛ.I x carrier).biQuiver(Ɛ.omega) {
            case(x, m) => Ɛ.truth(x)
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
              Ɛ.BiQuiver(morphisms x carrier, morphismMultiply).apply
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
        def apply[A <: ELEMENT](actionCarrier: Ɛ.STAR[A])(actionMultiply: (A, M) => A) =
          new RightActionStar(rightAction(actionCarrier)(actionMultiply))
      }

      class RightActionQuiver[S <: ELEMENT, T <: ELEMENT](
        val source: RightActionStar[S],
        val target: RightActionStar[T],
        val quiver: Ɛ.QUIVER[S, T]
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
      Wrappings[Ɛ.ELEMENT, RightAction, RightActionPrequiver] {
      override type ELEMENT = Ɛ.ElementProxy0[_]
      override type STAR[S <: ELEMENT] = RightActionStar[_, S]
      override type QUIVER[S <: ELEMENT, T <: ELEMENT] = RightActionQuiver[_, S, _, T]
      override type UNIT = Ɛ.ElementProxy0[Ɛ.UNIT]
      type RIGHT_IDEAL = Ɛ.>[M, Ɛ.TRUTH]
      override type TRUTH = Ɛ.ElementProxy0[RIGHT_IDEAL]
      override val I = RightActionStar(Ɛ.I) { (i, m) => i }

      private object RightIdeals {
        private val possibleIdeals = carrier.power
        private val isIdeal = (carrier x carrier).forAll(possibleIdeals) {
          case (f, (m, n)) => Ɛ.OmegaEnrichments(f(m)) > f(multiply(m, n))
        }
        private val ideals = possibleIdeals.toTrue ?= isIdeal

        def restrict[H <: Ɛ.ELEMENT](that: Ɛ.STAR[H])(bifunc: (H, M) => Ɛ.TRUTH): Ɛ.QUIVER[H, RIGHT_IDEAL] = 
          ideals.restrict(possibleIdeals.transpose(
            (that x carrier).biQuiver(Ɛ.omega)(bifunc)
          ))

        private val idealMultiply = restrict(ideals x carrier) {
            case ((i, s), t) => ideals.inclusion(i)(multiply(s, t))
          }
        val omega = RightActionStar[RIGHT_IDEAL](ideals)(
            Ɛ.BiQuiver[RIGHT_IDEAL, M, RIGHT_IDEAL](ideals x carrier, idealMultiply)(_, _)
          )
      }
      override lazy val omega = RightIdeals.omega

      override lazy val truth = 
        new RightActionQuiver(I, omega, carrier.power.transpose((Ɛ.I x carrier).biQuiver(Ɛ.omega) {
            case (x, m) => Ɛ.truth(x)
        }))

      class RightActionStar[A <: Ɛ.ELEMENT, E <: Ɛ.ElementProxy0[A]](
        private[RightMonoidActionsInDraft] val action: RightAction[A]
        ) extends Star[E] { star =>
        override val toI: QUIVER[E, UNIT] = 
          new RightActionQuiver(this, I, action.actionCarrier.toI)

        override def sanityTest = {
          action.actionCarrier.sanityTest
          action.sanityTest
        }

        // override def xUncached[B <: ELEMENT](that: STAR[B]): BIPRODUCT[E, B] = null
          // override def xUncached[B <: Ɛ.ELEMENT, F <: ELEMENT](that: RightActionStar[B, F]): BIPRODUCT[E, F] = {
        override def xUncached[F <: ELEMENT](that: STAR[F]): BIPRODUCT[E, F] = 
          that.preMultiplyUncached(this)

        private def preMultiplyUncached[Z <: Ɛ.ELEMENT, D <: Ɛ.ElementProxy0[Z]](pre: RightActionStar[Z, D]) : BIPRODUCT[D, E] = null
/*          
        private def preMultiplyUncached[Z <: Ɛ.ELEMENT, D <: Ɛ.ElementProxy0[Z]](pre: RightActionStar[Z, D]) : BIPRODUCT[D, E] = {
          val product = pre.action.actionCarrier x action.actionCarrier 
          val test1: (D, E) with Ɛ.ElementProxy0[Ɛ.x[Z, A]] = null.asInstanceOf[(D, E) with Ɛ.ElementProxy0[Ɛ.x[Z, A]]]
          val test2: D x E = test1
          val test3: D x E = null.asInstanceOf[D x E]
          val test4: (D, E) with Ɛ.ElementProxy0[Ɛ.x[Z, A]] = test3
          new RightActionStar[Ɛ.x[Z, A], (D, E) with Ɛ.ElementProxy0[Ɛ.x[Z, A]]] (
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
        def apply[A <: Ɛ.ELEMENT](actionCarrier: Ɛ.STAR[A])(actionMultiply: (A, M) => A) =
          new RightActionStar[A, Ɛ.ElementProxy0[A]](rightAction(actionCarrier)(actionMultiply))
      }

      class RightActionQuiver[S <: Ɛ.ELEMENT, E <: Ɛ.ElementProxy0[S], T <: Ɛ.ELEMENT, F <: Ɛ.ElementProxy0[T]](
        val source: RightActionStar[S, E],
        val target: RightActionStar[T, F],
        val quiver: Ɛ.QUIVER[S, T]
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

      override type WRAPPER[T <: Ɛ.ELEMENT] = Ɛ.ElementProxy0[T]

      private val memoizedStarWrapper = {
        type STAR_WRAPPER[T <: Ɛ.ELEMENT] = STAR[WRAPPER[T]]
        def wrap[T <: Ɛ.ELEMENT](input: RightAction[T]): STAR_WRAPPER[T] =
          new RightActionStar[T, WRAPPER[T]](input)
        Memoize.generic.withLowerBound[RightAction, STAR_WRAPPER, Ɛ.ELEMENT](wrap)
      }

      override def star[T <: Ɛ.ELEMENT](input: RightAction[T]): STAR[WRAPPER[T]] =
        memoizedStarWrapper(input)

      override def quiver[S <: Ɛ.ELEMENT, T <: Ɛ.ELEMENT](
        prequiver: RightActionPrequiver[S, T]
      ) = functionAsQuiver(
        star(prequiver.source), 
        star(prequiver.target), 
        prequiver.function
      )

      override def functionAsQuiver[S <: Ɛ.ELEMENT, T <: Ɛ.ELEMENT](
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
        L <: Ɛ.ELEMENT, 
        R <: Ɛ.ELEMENT, 
        T <: Ɛ.ELEMENT
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
          Ɛ.ElementProxy0(bifunc(a.element, b.element))
        }
      }
    }

    class RightMonoidActionsInDraft2 extends Topos with 
      Wrappings[Ɛ.ELEMENT, RightAction, RightActionPrequiver] {
      override type ELEMENT = Ɛ.ElementWrapper[_ <: Ɛ.ELEMENT]
      override type STAR[E <: ELEMENT] = RightActionStarFacade[E]
      override type QUIVER[E <: ELEMENT, F <: ELEMENT] = RightActionQuiverFacade[E, F]
      override type UNIT = Ɛ.ElementWrapper[Ɛ.UNIT]
      type RIGHT_IDEAL = Ɛ.>[M, Ɛ.TRUTH]
      override type TRUTH = Ɛ.ElementWrapper[RIGHT_IDEAL]
      override val I = RightActionStar(Ɛ.I) { (i, m) => i }

      private object RightIdeals {
        private val possibleIdeals = carrier.power
        private val isIdeal = (carrier x carrier).forAll(possibleIdeals) {
          case (f, (m, n)) => Ɛ.OmegaEnrichments(f(m)) > f(multiply(m, n))
        }
        private val ideals = possibleIdeals.toTrue ?= isIdeal

        def restrict[H <: Ɛ.ELEMENT](that: Ɛ.STAR[H])(bifunc: (H, M) => Ɛ.TRUTH): Ɛ.QUIVER[H, RIGHT_IDEAL] = 
          ideals.restrict(possibleIdeals.transpose(
            (that x carrier).biQuiver(Ɛ.omega)(bifunc)
          ))

        private val idealMultiply = restrict(ideals x carrier) {
            case ((i, s), t) => ideals.inclusion(i)(multiply(s, t))
          }
        val omega = RightActionStar[RIGHT_IDEAL](ideals)(
            Ɛ.BiQuiver[RIGHT_IDEAL, M, RIGHT_IDEAL](ideals x carrier, idealMultiply)(_, _)
          )
      }
      override lazy val omega = RightIdeals.omega
      override lazy val truth = 
        new RightActionQuiver(I, omega, carrier.power.transpose((Ɛ.I x carrier).biQuiver(Ɛ.omega) {
            case (x, m) => Ɛ.truth(x)
        }))

      trait ActionReceiver[AA, H] {
        def receive[A <: Ɛ.ELEMENT](actionStar: RightActionStar[A], Δ: A ↔ AA): H
      }

      trait RightActionStarFacade[AA <: ELEMENT] extends Star[AA] { facade => 
        private[RightMonoidActionsInDraft2] def preMultiplyUncached[Z <: Ɛ.ELEMENT, ZZ <: ELEMENT](
          pre: RightActionStar[Z] with RightActionStarFacade[ZZ]
        ) : BIPRODUCT[ZZ, AA]
        private[RightMonoidActionsInDraft2] def preExponentiateUncached[Z <: Ɛ.ELEMENT, ZZ <: ELEMENT](
          pre: RightActionStar[Z] with RightActionStarOuterFacade[Z, ZZ]
        ) : EXPONENTIAL[ZZ, AA]
        private[RightMonoidActionsInDraft2] def withAction[H](receiver: ActionReceiver[AA, H]): H
      }

      private class LaxRightActionStarFacade[AA <: ELEMENT, BB <: ELEMENT](
          delegate: RightActionStarFacade[AA],
          Δ: AA ↔ BB
        ) extends RightActionStarFacade[BB] {
        lazy val toI: QUIVER[BB, UNIT] =
          new LeftLaxRightActionQuiverFacade(delegate.toI, Δ, this)
        def xUncached[TT <: ELEMENT](that: STAR[TT]): BIPRODUCT[BB, TT] =
          new LeftLaxBiproduct(delegate.xUncached(that), Δ, this)
        def `>Uncached`[TT <: ELEMENT](that: STAR[TT]): EXPONENTIAL[BB, TT] =
          new LeftLaxExponentialStar(delegate.`>Uncached`(that), Δ, this)
        def apply[TT <: ELEMENT](target: STAR[TT])(f: BB => TT) : QUIVER[BB, TT] = 
          new LeftLaxRightActionQuiverFacade(delegate(target)(f compose (Δ./)), Δ, this)
        def sanityTest = delegate.sanityTest

        override private[RightMonoidActionsInDraft2] def preMultiplyUncached[Z <: Ɛ.ELEMENT, ZZ <: ELEMENT](
          pre: RightActionStar[Z] with RightActionStarFacade[ZZ]
        ) : BIPRODUCT[ZZ, BB] = 
          new RightLaxBiproduct(delegate.preMultiplyUncached[Z, ZZ](pre), Δ, this)

        private[RightMonoidActionsInDraft2] def preExponentiateUncached[Z <: Ɛ.ELEMENT, ZZ <: ELEMENT](
          pre: RightActionStar[Z] with RightActionStarOuterFacade[Z, ZZ]
        ) : EXPONENTIAL[ZZ, BB] =
          new RightLaxExponentialStar(delegate.preExponentiateUncached[Z, ZZ](pre), Δ, this)

        private[RightMonoidActionsInDraft2] def withAction[H](receiver: ActionReceiver[BB, H]): H =
          delegate.withAction(new ActionReceiver[AA, H] {
            override def receive[A <: Ɛ.ELEMENT](actionStar: RightActionStar[A], Ψ: A ↔ AA): H =
              receiver.receive(actionStar, Ψ o Δ) // A ↔ BB from Ψ: A ↔ AA and Δ: AA ↔ BB)
          })
      }

      private class RightLaxBiproduct[
        AA <: ELEMENT,
        BB <: ELEMENT, 
        CC <: ELEMENT
      ] (
        delegate: BIPRODUCT[AA, BB],
        Δ: BB ↔ CC,
        laxStar: STAR[CC]
      ) extends LaxRightActionStarFacade[AA x BB, AA x CC](delegate,
        new ↔[AA x BB, AA x CC] (
          dxe => rightLaxPair(dxe._1, Δ / dxe._2, delegate, Δ),
          dxf => delegate.pair(dxf._1, Δ \ dxf._2)
        )
      ) with BiproductStar[AA, CC] {
        val left: STAR[AA] = delegate.left
        val right: STAR[CC] = laxStar
        def pair(aa: AA, cc: CC): AA x CC = rightLaxPair(aa, cc, delegate, Δ)
      }

      private def leftLaxPair[
        E <: ELEMENT, 
        F <: ELEMENT,
        G <: ELEMENT
      ] (f: F, g: G, biproduct: BIPRODUCT[E, G], Δ: E ↔ F) : F x G = 
          biproduct.pair(Δ \ f, g)(f, g)

      private def rightLaxPair[
        D <: ELEMENT,
        E <: ELEMENT, 
        F <: ELEMENT
      ] (d: D, f: F, biproduct: BIPRODUCT[D, E], Δ: E ↔ F) : D x F = 
          biproduct.pair(d, Δ \ f)(d, f)

      private class LeftLaxBiproduct[
        E <: ELEMENT, 
        F <: ELEMENT,
        G <: ELEMENT
      ] (
        delegate: BIPRODUCT[E, G],
        Δ: E ↔ F,
        laxStar: STAR[F]
      ) extends LaxRightActionStarFacade[E x G, F x G](delegate, 
        new ↔[E x G, F x G](
          exg => leftLaxPair(Δ / (exg._1), exg._2, delegate, Δ),
          fxg => delegate.pair(Δ \ (fxg._1), fxg._2)
        )
      ) with BiproductStar[F, G] {
        val left: STAR[F] = laxStar
        val right: STAR[G] = delegate.right
        def pair(f: F, g: G): F x G = leftLaxPair(f, g, delegate, Δ)
      }

      private def leftLaxExponential[
        E <: ELEMENT, 
        F <: ELEMENT,
        G <: ELEMENT
      ] (e2g: E > G, Δ: E ↔ F) : F > G = 
        e2g { (f: F) => e2g(Δ \ f) }

      private def rightLaxExponential[
        D <: ELEMENT, 
        E <: ELEMENT,
        F <: ELEMENT
      ] (d2e: D > E, Δ: E ↔ F) : D > F = 
        d2e { (d: D) =>  Δ / d2e(d) }

      private class LeftLaxExponentialStar[
        E <: ELEMENT, 
        F <: ELEMENT,
        G <: ELEMENT
      ] (
        delegate: EXPONENTIAL[E, G],
        Δ: E ↔ F,
        laxStar: STAR[F]
      ) extends LaxRightActionStarFacade[E > G, F > G](delegate,
        new ↔[E > G, F > G](
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
        D <: ELEMENT, 
        E <: ELEMENT,
        F <: ELEMENT
      ] (
        delegate: EXPONENTIAL[D, E],
        Δ: E ↔ F,
        laxStar: STAR[F]
      ) extends LaxRightActionStarFacade[D > E, D > F](delegate,
        new ↔[D > E, D > F](
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

      trait RightActionStarOuterFacade[A <: Ɛ.ELEMENT, AA <: ELEMENT] extends RightActionStarFacade[AA] {
        val equivalence: A ↔ AA
      }

      class RightActionStar[A <: Ɛ.ELEMENT](private[RightMonoidActionsInDraft2] val action: RightAction[A]) extends
        RightActionStarOuterFacade[A, Ɛ.ElementWrapper[A]] { star =>
        private type AA = Ɛ.ElementWrapper[A]

        override val toI: QUIVER[AA, UNIT] = 
          new RightActionQuiver(this, I, action.actionCarrier.toI)

        override def sanityTest = {
          action.actionCarrier.sanityTest
          action.sanityTest
        }

        override def xUncached[F <: ELEMENT](that: STAR[F]): BIPRODUCT[AA, F] = 
          that.preMultiplyUncached[A, AA](this)

        override private[RightMonoidActionsInDraft2] def 
          preMultiplyUncached[Z <: Ɛ.ELEMENT, D <: ELEMENT](
          pre: RightActionStar[Z] with RightActionStarFacade[D]
        ) : BIPRODUCT[D, AA] = {
          val product = pre.action.actionCarrier x action.actionCarrier 
          val innerStar = new RightActionStar[Ɛ.x[Z, A]] (
            rightAction(product){
              case ((z, a), m) => product.pair(
                  pre.action.actionMultiply(z, m),
                  action.actionMultiply(a, m) 
                )
              })
          val innerBiproduct: Ɛ.BIPRODUCT[Z, A] = pre.action.actionCarrier x action.actionCarrier
          val Δ = new ↔[Ɛ.ElementWrapper[Ɛ.x[Z, A]], D x AA](
            zxa => zxa.element match { case (z, a) =>
              zxa(
                  Ɛ.ElementWrapper[Z](z).asInstanceOf[D], // TODO: fix cast
                  Ɛ.ElementWrapper[A](a) // TODO: shouldn't need all these generic args
                )
              },
              dxe => Ɛ.ElementWrapper(dxe.element.asInstanceOf[Ɛ.x[Z, A]]) // TODO: fix cast?
            )
          new LaxRightActionStarFacade(innerStar, Δ) with BiproductStar[D, AA] {
            override val left: STAR[D] = pre
            override val right: STAR[AA] = star
            override def pair(d: D, e: AA): x[D, AA] = { 
              val z: Z = d.element.asInstanceOf[Z] // TODO: fix cast
              val a: A = e.element
              Ɛ.ElementWrapper(innerBiproduct.pair(z, a))(d, e)
            }
          }
        }

        override def `>Uncached`[TT <: ELEMENT](that: STAR[TT]): EXPONENTIAL[AA, TT] =  
          that.preExponentiateUncached[A, AA](this)

        override private[RightMonoidActionsInDraft2] def 
          preExponentiateUncached[Z <: Ɛ.ELEMENT, ZZ <: ELEMENT](
          pre: RightActionStar[Z] with RightActionStarOuterFacade[Z, ZZ]
        ) : EXPONENTIAL[ZZ, AA] = {
            val outerFacade: RightActionStarOuterFacade[Z, ZZ] = pre
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
            type P = Ɛ.>[Ɛ.x[M, Z],A]
            type PP = Ɛ.ElementWrapper[P]

            val innerExpStar: STAR[PP] = new RightActionStar[P](rightAction(morphisms)(
              Ɛ.BiQuiver(morphisms x carrier, morphismMultiply).apply
            )) 
            val Δ = new ↔[PP, ZZ > AA](
              pp => pp { (zz: ZZ) => {
                val z = outerFacade.equivalence \ zz
                val p: P = pp.element
                val unitM: M = unit(pre.action.actionCarrier.toI(z))
                val a: A = p(pairs.pair(unitM, z))
                Ɛ.ElementWrapper(a)
              }},
              zz2aa => {
                val p = zz2aa.element.asInstanceOf[P] // TODO: fix cast? Need topos API method to adjust elements here?
                Ɛ.ElementWrapper(p)
              }
            )
            new LaxRightActionStarFacade(innerExpStar, Δ) with ExponentialStar[ZZ, AA] { exponentialStar =>
              val source: STAR[ZZ] = pre
              val target: STAR[AA] = star
              def transpose[RR <: ELEMENT](biQuiver: BiQuiver[RR, ZZ, AA]): QUIVER[RR, ZZ > AA] = {
                val lhs: STAR[RR] = biQuiver.product.left
                lhs.withAction(new ActionReceiver[RR, QUIVER[RR, ZZ > AA]] {
                  override def receive[R <: Ɛ.ELEMENT](lhsActionStar: RightActionStar[R], Ψ: R ↔ RR) = {
                    val innerQuiver: Ɛ.QUIVER[R, P] =
                      morphisms.restrict(possibleMorphisms.transpose(
                        (lhsActionStar.action.actionCarrier x pairs).biQuiver(action.actionCarrier) {
                          case (r, (m, z)) => 
                            val rr: RR = Ψ / lhsActionStar.action.actionMultiply(r, m)
                            val zz: ZZ = outerFacade.equivalence / z
                            equivalence \ biQuiver(rr, zz)
                          }))
                      lhs(exponentialStar) { rr =>
                         val p: P = innerQuiver(Ψ \ rr)
                         val pp = Ɛ.ElementWrapper(p)
                         Δ / pp
                      }
                  }})
              }
            }
          }

        override def apply[TT <: ELEMENT](target: STAR[TT])(f: AA => TT): QUIVER[AA, TT] = null

        override val equivalence: A ↔ AA = Ɛ.ElementWrapper.↔[A]
        override def toString = "RightActionStar[" + action.actionCarrier + "]"
        override private[RightMonoidActionsInDraft2] def withAction[H](receiver: ActionReceiver[AA, H]): H =
          receiver.receive(star, equivalence)
      }

      object RightActionStar {
        def apply[A <: Ɛ.ELEMENT](actionCarrier: Ɛ.STAR[A])(actionMultiply: (A, M) => A) =
          new RightActionStar[A](rightAction(actionCarrier)(actionMultiply))
      }

      trait RightActionQuiverFacade[E <: ELEMENT, F <: ELEMENT] extends
        Quiver[E, F] { facade =>
      }

      // TODO: do we need this? Can a quiver just be between StarFacades?
      class LeftLaxRightActionQuiverFacade[
        E <: ELEMENT, 
        F <: ELEMENT,
        G <: ELEMENT
      ](
          delegate: RightActionQuiverFacade[E, G],
          Δ: E ↔ F,
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

      class RightActionQuiver[A <: Ɛ.ELEMENT, B <: Ɛ.ELEMENT](
        val source: RightActionStar[A],
        val target: RightActionStar[B],
        val quiver: Ɛ.QUIVER[A, B]
      ) extends RightActionQuiverFacade[Ɛ.ElementWrapper[A], Ɛ.ElementWrapper[B]] {
        private type E = Ɛ.ElementWrapper[A]
        private type F = Ɛ.ElementWrapper[B]

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

      override type WRAPPER[T <: Ɛ.ELEMENT] = Ɛ.ElementWrapper[T]

      private val memoizedStarWrapper = {
        type STAR_WRAPPER[T <: Ɛ.ELEMENT] = STAR[WRAPPER[T]]
        def wrap[T <: Ɛ.ELEMENT](input: RightAction[T]): STAR_WRAPPER[T] =
          null // new RightActionStar[T, WRAPPER[T]](input)
        Memoize.generic.withLowerBound[RightAction, STAR_WRAPPER, Ɛ.ELEMENT](wrap)
      }

      override def star[T <: Ɛ.ELEMENT](input: RightAction[T]): STAR[WRAPPER[T]] =
        null // memoizedStarWrapper(input)

      override def quiver[S <: Ɛ.ELEMENT, T <: Ɛ.ELEMENT](
        prequiver: RightActionPrequiver[S, T]
      ) = functionAsQuiver(
        star(prequiver.source), 
        star(prequiver.target), 
        prequiver.function
      )

      override def functionAsQuiver[S <: Ɛ.ELEMENT, T <: Ɛ.ELEMENT](
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
        L <: Ɛ.ELEMENT, 
        R <: Ɛ.ELEMENT, 
        T <: Ɛ.ELEMENT
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
        //   Ɛ.ElementProxy0(bifunc(a.element, b.element))
        // }
      }
    }
  }
}

