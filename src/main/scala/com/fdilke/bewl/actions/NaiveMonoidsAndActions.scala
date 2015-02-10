package com.fdilke.bewl.actions

import com.fdilke.bewl.topos._
import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.helper.↔

// Monoids and monoid actions defined as 'one-off' structures.
// TODO: once more general machinery is in place, update all monoid code and deprecate this

trait NaiveMonoidsAndActions { Ɛ: BaseTopos with AlgebraicMachinery with LogicalOperations =>

  case class ElementProxy0[A <: ELEMENT](element: A)

  trait ElementWrapper[A <: ELEMENT] { wrapper =>
    type BASE = A
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
      override type STAR[AA <: ELEMENT] = RightActionStarFacade[AA]
      override type QUIVER[AA <: ELEMENT, BB <: ELEMENT] = RightActionQuiverFacade[AA, BB]
      override type UNIT = Ɛ.ElementWrapper[Ɛ.UNIT]

      // experimental: tighten up the exponential type
      // override type >[SS <: ELEMENT, TT <: ELEMENT] = (SS => TT) with Ɛ.ElementWrapper[Ɛ.>[Ɛ.x[M, SS#BASE], TT#BASE]]

      type RIGHT_IDEAL = Ɛ.>[M, Ɛ.TRUTH]
      override type TRUTH = Ɛ.ElementWrapper[RIGHT_IDEAL]
      override val I: RightActionStar[Ɛ.UNIT, UNIT] = RightActionStar(Ɛ.I) { (i, m) => i }

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
      override lazy val truth: QUIVER[UNIT, TRUTH] = 
        new RightActionQuiver[Ɛ.UNIT, UNIT, RIGHT_IDEAL, TRUTH](I, omega, // TODO: need generics?
          carrier.power.transpose((Ɛ.I x carrier).biQuiver(Ɛ.omega) {
            case (x, m) => Ɛ.truth(x)
        }))

      trait ActionReceiver[AA <: ELEMENT, H] {
        def receive[A <: Ɛ.ELEMENT](actionStar: RightActionStar[A, AA]): H
      }

      trait RightActionStarFacade[AA <: ELEMENT] extends Star[AA] { facade => 
        private[RightMonoidActionsInDraft2] def reveal[H](receiver: ActionReceiver[AA, H]): H
      }

      class RightActionStar[A <: Ɛ.ELEMENT, AA <: ELEMENT](
        private[RightMonoidActionsInDraft2] val action: RightAction[A],
        private[RightMonoidActionsInDraft2] val ↔ : A ↔ AA = Ɛ.ElementWrapper.↔[A] // TODO: bake into interface?
      ) extends RightActionStarFacade[AA] { star =>

        override val toI: QUIVER[AA, UNIT] = 
          new RightActionQuiver[A, AA, Ɛ.UNIT, UNIT](this, I, action.actionCarrier.toI) // TODO: need generics?

        override def sanityTest = {
          action.actionCarrier.sanityTest
          action.sanityTest
        }

        override def xUncached[FF <: ELEMENT](thatFacade: STAR[FF]): BIPRODUCT[AA, FF] = 
          thatFacade.reveal(new ActionReceiver[FF, BIPRODUCT[AA, FF]] {
            override def receive[F <: Ɛ.ELEMENT](that: RightActionStar[F, FF]) = {            
              val product = action.actionCarrier x that.action.actionCarrier 
              new RightActionStar[Ɛ.x[A, F], AA x FF] (
                  rightAction(product){
                    case ((a, f), m) => product.pair(
                        action.actionMultiply(a, m),
                        that.action.actionMultiply(f, m) 
                      )
                    }, 
                  new ↔[Ɛ.x[A, F], AA x FF](
                    axf => axf match { case (a, f) =>
                      Ɛ.ElementWrapper(axf)(↔ / a, that.↔ / f)
                    },
                    aaXff => aaXff.element.asInstanceOf[Ɛ.x[A, F]] // TODO: fix cast?
                )) with BiproductStar[AA, FF] {
                  override val left: STAR[AA] = star
                  override val right: STAR[FF] = that
                  override def pair(aa: AA, ff: FF): x[AA, FF] = { 
                    val a: A = star.↔ \ aa
                    val f: F = that.↔ \ ff
                    Ɛ.ElementWrapper(product.pair(a, f))(aa, ff)
                }}}})

        override def `>Uncached`[TT <: ELEMENT](thatFacade: STAR[TT]): EXPONENTIAL[AA, TT] =  
          thatFacade.reveal(new ActionReceiver[TT, EXPONENTIAL[AA, TT]] {
            override def receive[T <: Ɛ.ELEMENT](that: RightActionStar[T, TT]) = {            
              val pairs = carrier x action.actionCarrier
              val possibleMorphisms = pairs > that.action.actionCarrier
              val isMorphism = (pairs x carrier).forAll(possibleMorphisms) {
                case (f, ((m, a), n)) =>
                  that.action.actionCarrier.diagonal(
                    f(pairs.pair(multiply(m, n), action.actionMultiply(a, n))),
                    that.action.actionMultiply(f(pairs.pair(m, a)), n)
                  )
              }
              val morphisms = possibleMorphisms.toTrue ?= isMorphism
              val morphismMultiply = morphisms.restrict(possibleMorphisms.transpose(
                (morphisms x carrier x pairs).biQuiver(that.action.actionCarrier) {
                  case ((f, m), (n, a)) => morphisms.inclusion(f)(
                    pairs.pair(multiply(m, n), a)
                )}))
              type P = Ɛ.>[Ɛ.x[M, A],T]
  // would need these to make it all properly typesafe :
  //             type Q = Ɛ.>[Ɛ.x[M, AA#BASE], TT#BASE]
              // def q2p(q: Q): P = q.reskin { (mxa: Ɛ.x[M, A]) => mxa match {
              //   case (m, a) => that.↔ / q(mxa.reskin(m, (↔ / a).element)
              // }} 
              // def p2q(p: P): Q = p.reskin { (mxb: Ɛ.x[M, AA#BASE]) => mxb match {
              //   case (m, b) => ? ↔ \ p(mxb.reskin(m, (? \ b).element)
              // }}
              type PP = Ɛ.ElementWrapper[P]
              def p2aa2tt(p: P) = Ɛ.ElementWrapper(p) { 
                (aa: AA) => {
                  val a = ↔ \ aa
                  val unitM: M = unit(action.actionCarrier.toI(a))
                  val t: T = p(pairs.pair(unitM, a))
                  that.↔ / t
                }}
              new RightActionStar[P, AA > TT](rightAction(morphisms) {
                  Ɛ.BiQuiver(morphisms x carrier, morphismMultiply)(_,_)
                }, 
                new ↔[P, AA > TT](
                  p2aa2tt,
                  aa2tt => aa2tt.element.asInstanceOf[P] // TODO: fix cast? Need topos API method to adjust elements here?
                )
              ) with ExponentialStar[AA, TT] { exponentialStar =>
                val source: STAR[AA] = star
                val target: STAR[TT] = that
                def transpose[RR <: ELEMENT](biQuiver: BiQuiver[RR, AA, TT]): QUIVER[RR, AA > TT] = {
                  val lhsFacade: STAR[RR] = biQuiver.product.left // TODO: inline
                  lhsFacade.reveal(new ActionReceiver[RR, QUIVER[RR, AA > TT]] {
                    override def receive[R <: Ɛ.ELEMENT](lhs: RightActionStar[R, RR]) = {
                      val innerQuiver: Ɛ.QUIVER[R, P] =
                        morphisms.restrict(possibleMorphisms.transpose(
                          (lhs.action.actionCarrier x pairs).biQuiver(that.action.actionCarrier) {
                            case (r, (m, a)) => 
                              val rr: RR = lhs.↔ / lhs.action.actionMultiply(r, m)
                              val aa: AA = star.↔ / a
                              that.↔ \ biQuiver(rr, aa)
                            }))
                        lhs(exponentialStar) { rr =>
                           val p: P = innerQuiver(lhs.↔ \ rr)
                           p2aa2tt(p)
                        }}})}}}})

        override def apply[TT <: ELEMENT](targetFacade: STAR[TT])(f: AA => TT): QUIVER[AA, TT] = 
          targetFacade.reveal(new ActionReceiver[TT, QUIVER[AA, TT]] {
            override def receive[T <: Ɛ.ELEMENT](target: RightActionStar[T, TT]) = 
              new RightActionQuiver[A, AA, T, TT](star, target, // TODO: need generics?
                star.action.actionCarrier(target.action.actionCarrier) { a =>
                  val aa = star.↔ / a
                  val tt = f(aa)
                  target.↔ \ tt    // TODO: condense
                })})

        override def toString = "RightActionStar[" + action.actionCarrier + "]"
        override private[RightMonoidActionsInDraft2] def reveal[H](
          receiver: ActionReceiver[AA, H]
        ): H = receiver.receive(star)
      }

      object RightActionStar {
        def apply[A <: Ɛ.ELEMENT](actionCarrier: Ɛ.STAR[A])(actionMultiply: (A, M) => A) =
          new RightActionStar[A, Ɛ.ElementWrapper[A]](
            rightAction(actionCarrier)(actionMultiply),
            Ɛ.ElementWrapper.↔[A]
          )
      }

      // TODO: will presumably be fleshed out as I populate the quiver API
      trait RightActionQuiverFacade[E <: ELEMENT, F <: ELEMENT] extends
        Quiver[E, F] { facade =>
      }

      class RightActionQuiver[A <: Ɛ.ELEMENT, AA <: ELEMENT, B <: Ɛ.ELEMENT, BB <: ELEMENT](
        val source: RightActionStar[A, AA],
        val target: RightActionStar[B, BB],
        val quiver: Ɛ.QUIVER[A, B]
      ) extends RightActionQuiverFacade[AA, BB] {

        override lazy val chi: QUIVER[BB, TRUTH] = null

        override def \[UU <: ELEMENT](monic: QUIVER[UU, BB]) = null

        override def sanityTest = {
          quiver.sanityTest
          assert(source.action.isMorphism(target.action, quiver))
        }

        override def ?=(that: QUIVER[AA, BB]): EQUALIZER[AA] = null

        override def apply(e: AA) = null.asInstanceOf[BB]

        override def o[ZZ <: ELEMENT](that: QUIVER[ZZ, AA]): QUIVER[ZZ, BB] = null

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

