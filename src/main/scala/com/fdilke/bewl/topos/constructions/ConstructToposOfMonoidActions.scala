package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.topos._
import com.fdilke.bewl.helper.Memoize
import com.fdilke.bewl.topos.algebra.{AlgebraicMachinery, AlgebraicStructures}
import com.fdilke.bewl.helper.{⊕, Memoize}
import scala.language.reflectiveCalls

trait ConstructToposOfMonoidActions
  extends BaseTopos
  with ToposEnrichments
  with ConstructDefaultMonoidAssistant {

  Ɛ: ToposPrerequisites =>

  object ToposOfMonoidActions {
    def of[
      M <: ~
    ](
      monoid: Monoid[M],
      assistant: MonoidAssistant = monoidAssistant
    ): Topos[~]
      with Wrappings[
        ~,
        ~,
        ({ type λ[X <: ~] = monoid.Action[X] })#λ,
        ({ type λ[X <: ~, Y <: ~] = monoid.ActionPreArrow[X, Y] })#λ,
        ({ type λ[T <: ~] = T })#λ
      ] =
      new Topos[~]
        with Wrappings[
          ~,
          ~,
          ({ type λ[X <: ~] = monoid.Action[X] })#λ,
          ({ type λ[X <: ~, Y <: ~] = monoid.ActionPreArrow[X, Y] })#λ,
          ({ type λ[T <: ~] = T })#λ
        ] {
        override val name = "MonoidActions in " + Ɛ.name

        import monoid.{action, carrier, Action}

        val analyzer: monoid.ActionAnalyzer =
          assistant.actionAnalyzer(monoid)

        override type DOT[A <: ~] = ActionDot[A]
        override type >[A <: ~, B <: ~] = ActionArrow[A, B]
        override type UNIT = Ɛ.UNIT
        override type →[T <: ~, U <: ~] = Ɛ.→[Ɛ.x[M, T], U]

        type IDEAL = Ɛ.→[M, Ɛ.TRUTH]
        override type TRUTH = IDEAL

        override lazy val I =
          new ActionDot(
            action(Ɛ.I)((i, m) => i)
          )
        override lazy val optionalGenerator =
          Some(new ActionDot(monoid.regularAction))

        private object Ideals {
          private val possibleIdeals = carrier.power
          import possibleIdeals.{evaluate => $}

          private val ideals =
            possibleIdeals.whereAll(carrier, carrier) { (f, m, n) =>
              Ɛ.OmegaEnrichments($(f, m)) →
                $(f, monoid.multiply(m, n))
            }

          def restrict[
            H <: ~
          ](
            that: Ɛ.DOT[H]
          )(
            bifunc: (H, M) => Ɛ.TRUTH
          ): Ɛ.>[H, IDEAL] =
            ideals.restrict(
              possibleIdeals.transpose(that)(bifunc)
            )

          private val idealMultiply =
            restrict(ideals.x(carrier)) {
              case (i ⊕ s, t) =>
                $(ideals.inclusion(i), monoid.multiply(s, t))
            }

          val idealJoin: Ɛ.>[Ɛ.x[IDEAL, IDEAL], IDEAL] =
            restrict(ideals.x(ideals)) {
              case (i ⊕ j, t) =>
                Ɛ.logicalOperations.or(
                  $(ideals.inclusion(i), t),
                  $(ideals.inclusion(j), t)
                )
            }

          val idealFalsity: Ɛ.>[Ɛ.UNIT, IDEAL] =
            restrict(Ɛ.I) {
              case (u, t) =>
                Ɛ.falsity(u)
            }

          val omega =
            new ActionDot[IDEAL](
              Action(
                ideals,
                Ɛ.BiArrow[IDEAL, M, IDEAL](
                  ideals.x(carrier),
                  idealMultiply
                )
              )
            )
        }

        override lazy val omega =
          Ideals.omega

        override lazy val truth =
          ActionArrow[Ɛ.UNIT, IDEAL](
            I,
            omega,
            Ideals.restrict(Ɛ.I)((i, m) => Ɛ.truth(i))
          )

        class ActionDot[
          S <: ~
        ](
          val action: Action[S]
        ) extends Dot[S] { actionDot =>
          lazy val analysis =
            analyzer.analyze(action)

          override def >>[T <: ~](
            target: ActionDot[T]
          ): Iterable[
            S > T
          ] =
            analysis
              .morphismsTo(
                target.analysis
              )
              .map { arrow =>
                ActionArrow(
                  actionDot,
                  target,
                  arrow
                )
              }

          override def `>Uncached`[
            T <: ~
          ](
            that: ActionDot[T]
          ): EXPONENTIAL[S, T] = {
            val rawExponential =
              analysis.rawExponential(
                that.analysis
              )

            new ActionDot[S → T](
              rawExponential.exponentialAction
            ) with ExponentialDot[S, T] { exponentialDot =>
              override val source = actionDot
              override val target = that
              override def evaluate(
                function: S → T,
                s: S
              ): T =
                rawExponential.evaluation(function, s)

              override def transpose[
                R <: ~
              ](
                biArrow: BiArrow[R, S, T]
              ): ActionArrow[R, S → T] = {
                val otherAction = biArrow.product.left
                val biProduct =
                  otherAction.action.actionCarrier.x(actionDot.action.actionCarrier)
                ActionArrow(
                  otherAction,
                  exponentialDot,
                  rawExponential.transpose(
                    otherAction.action,
                    Ɛ.BiArrow(
                      biProduct,
                      biArrow.arrow.arrow
                    )
                  )
                )
              }
            }
          }

          override def apply[
            T <: ~
          ](
            target: ActionDot[T]
          )(
            f: S => T
          ): S > T =
            ActionArrow[S, T](
              actionDot,
              target,
              action.carrier(
                target.action.carrier
              )(f)
            )

          override lazy val globals: Iterable[UNIT > S] = {
            val fixedPoints =
              action.actionCarrier.whereAll(
                carrier
              ) { (a, m) =>
                action.actionCarrier.=?=(
                  a,
                  action.actionMultiply(a, m)
                )
              }

            fixedPoints.globals.map { global =>
              ActionArrow(
                I,
                actionDot,
                fixedPoints.inclusion.o(global)
              )
            }
          }

          override def sanityTest: Unit = {
            action.actionCarrier.sanityTest
            action.sanityTest
          }

          override lazy val toI: S > UNIT =
            ActionArrow(
              actionDot,
              I,
              action.actionCarrier.toI
            )

          override def xUncached[
            T <: ~
          ](
            that: ActionDot[T]
          ): BIPRODUCT[S, T] = {
            val productDot: Ɛ.BIPRODUCT[S, T] =
              action.actionCarrier.x(that.action.actionCarrier)

            new ActionDot[S x T](
              this.action.x(that.action)
            ) with BiproductDot[S, T] {
              override val left = actionDot
              override val right = that
              def pair(l: S, r: T) =
                productDot.pair(l, r)
            }
          }
        }

        case class ActionArrow[
          S <: ~,
          T <: ~
        ](
          source: ActionDot[S],
          target: ActionDot[T],
          arrow: Ɛ.>[S, T]
        ) extends Arrow[S, T] {
          def this(
            source: ActionDot[S],
            target: ActionDot[T],
            function: S => T
          ) =
            this(
              source,
              target,
              source.action.carrier(
                target.action.carrier
              )(
                function
              )
            )

          override def \[
            U <: ~
          ](
            monic: U > T
          ): S > U =
            ActionArrow[S, U](
              source,
              monic.source,
              arrow \ monic.arrow
            )

          override def ?=(
            that: S > T
          ): EQUALIZER[S] = {
            val equalizer: Ɛ.EQUALIZER[S] =
              arrow ?= that.arrow
            val equalizerCarrier =
              equalizer.x(carrier)
            val restrictedMultiply =
              equalizer.restrict(
                equalizerCarrier
                  .biArrow(
                    source.action.carrier
                  ) { (s, m) =>
                    source.action.actionMultiply(
                      equalizer.inclusion(s),
                      m
                    )
                  }
                  .arrow
              )
            new ActionDot(
              action(
                equalizer
              ) { (s, m) =>
                restrictedMultiply(
                  equalizerCarrier.pair(s, m)
                )
              }
            ) with EqualizingDot[S] { equalizingDot =>
              val equalizerTarget = source
              def restrict[
                R <: ~
              ](
                actionArrow: ActionArrow[R, S]
              ) =
                ActionArrow[R, S](
                  actionArrow.source,
                  equalizingDot,
                  equalizer.restrict(
                    actionArrow.arrow
                  )
                )
            }
          }

          override def apply(
            s: S
          ): T =
            arrow(s)

          override lazy val chi: T > TRUTH =
            ActionArrow(
              target,
              omega,
              Ideals.restrict(
                target.action.actionCarrier
              ) { (t, m) =>
                arrow.chi(
                  target.action.actionMultiply(t, m)
                )
              }
            )

          override def o[
            R <: ~
          ](
            that: R > S
          ): R > T =
            ActionArrow[R, T](
              that.source,
              target,
              arrow.o(that.arrow)
            )

          override def sanityTest: Unit = {
            arrow.sanityTest
            assert(
              monoid.actions.isMorphism(
                source.action,
                target.action,
                arrow
              )
            )
          }
        }

        override val imageFinder: ImageFinder =
          new ImageFinder {
            def image[
              S <: ~,
              T <: ~
            ](
              arrow: S > T
            ): EQUALIZER[T] = {
              val delegatedImage =
                arrow.arrow.image

              new ActionDot(
                action(
                  delegatedImage
                ) { (s, m) =>
                  arrow.target.action.actionMultiply(
                    s,
                    m
                  )
                }
              ) with EqualizingDot[T] { equalizingDot =>
                override val equalizerTarget = arrow.target
                override def restrict[
                  R <: ~
                ](
                  actionArrow: ActionArrow[R, T]
                ) =
                  ActionArrow[R, T](
                    actionArrow.source,
                    equalizingDot,
                    delegatedImage.restrict(
                      actionArrow.arrow
                    )
                  )
              }
            }
          }

        override val logicalOperations: LogicalOperations =
          new DefaultLogicalOperations {
            override lazy val or =
              omega.squared.biArrow(omega) { (i, j) =>
                Ideals.idealJoin(
                  omega.squared.pair(i, j)
                )
              }
            override lazy val falsity =
              I(omega)(u => Ideals.idealFalsity(u))
          }

        override def bifunctionAsBiArrow[
          L <: ~,
          R <: ~,
          T <: ~
        ](
          left: ActionDot[L],
          right: ActionDot[R],
          target: ActionDot[T]
        )(
          bifunc: (L, R) => T
        ): BiArrow[L, R, T] = // no wrappings: generic version will do
          left.x(right).biArrow(target)(bifunc)

        override def functionAsArrow[
          S <: ~,
          T <: ~
        ](
          source: ActionDot[S],
          target: ActionDot[T],
          f: S => T
        ): ActionArrow[S, T] = // no wrappings: generic version will do
          source(target)(f)

        override def makeArrow[
          S <: ~,
          T <: ~
        ](
          preArrow: monoid.ActionPreArrow[S, T]
        ): ActionArrow[S, T] =
          ActionArrow[S, T](
            makeDot(preArrow.source),
            makeDot(preArrow.target),
            preArrow.source.actionCarrier(
              preArrow.target.actionCarrier
            ) {
              preArrow.function
            }
          )

        private val memoizedDotWrapper =
          Memoize.generic.withLowerBound[
            ({ type λ[T <: Ɛ.~] = Action[T] })#λ,
            ({ type λ[T <: Ɛ.~] = DOT[T] })#λ,
            Ɛ.~
          ](action => new ActionDot(action))

        override def makeDot[T <: ~](
          predot: monoid.Action[T]
        ): ActionDot[T] =
          memoizedDotWrapper(predot)

        override def unwrap[T <: ~](
          dot: ActionDot[T]
        ) =
          dot.action
      }
  }
}
