package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.helper.{⊕, Memoize}
import com.fdilke.bewl.topos.algebra.{AlgebraicMachinery, AlgebraicStructures}
import com.fdilke.bewl.topos._

trait ConstructToposOfGroupActions extends BaseTopos with ToposEnrichments {

  Ɛ: ToposPrerequisites =>

  object ToposOfGroupActions {
    def of[G <: ~](group: Ɛ.Group[G]): Topos[~]
      with Wrappings[
        ~,
        ~,
        ({ type λ[X <: ~] = group.Action[X] })#λ,
        ({ type λ[X <: ~, Y <: ~] = group.ActionPreArrow[X, Y] })#λ,
        ({ type λ[T <: ~] = T })#λ
      ] = {
      new Topos[~]
        with Wrappings[
          ~,
          ~,
          ({ type λ[X <: ~] = group.Action[X] })#λ,
          ({ type λ[X <: ~, Y <: ~] = group.ActionPreArrow[X, Y] })#λ,
          ({ type λ[T <: ~] = T })#λ
        ] {
        override val name = "GroupActions in " + Ɛ.name

        override type DOT[X <: ~] = ActionDot[X]
        override type >[X <: ~, Y <: ~] = ActionArrow[X, Y]
        override type UNIT = Ɛ.UNIT
        override type TRUTH = Ɛ.TRUTH
        override type →[T <: ~, U <: ~] = Ɛ.→[T, U]

        override val I: DOT[UNIT] =
          ActionDot(Ɛ.I)((i, g) => i)
        override lazy val optionalGenerator =
          Some(new ActionDot(group.regularAction))

        override lazy val omega: DOT[TRUTH] =
          ActionDot(Ɛ.omega)((ω, g) => ω)

        override lazy val truth: >[UNIT, TRUTH] =
          ActionArrow(I, omega, Ɛ.truth)

        class ActionDot[
          S <: ~
        ](
          val action: group.Action[S]
        ) extends Dot[S] { dot =>

          override val toI =
            ActionArrow(this, I, action.actionCarrier.toI)

          override lazy val globals = {
            val fixedPoints =
              action.actionCarrier.whereAll(
                group.carrier
              ) { (a, g) =>
                action.actionCarrier.=?=(
                  a,
                  action.actionMultiply(a, g)
                )
              }

            fixedPoints.globals.map { global =>
              ActionArrow(I, dot, fixedPoints.inclusion.o(global))
            }
          }

          override def xUncached[
            T <: ~
          ](
            that: DOT[T]
          ): BIPRODUCT[S, T] = {
            val productDot: Ɛ.BIPRODUCT[S, T] =
              this.action.actionCarrier.x(that.action.actionCarrier)

            new ActionDot[S x T](
              this.action.x(that.action)
            ) with BiproductDot[S, T] {
              override val left = dot
              override val right = that
              override def pair(s: S, t: T) =
                productDot.pair(s, t)
            }
          }

          override def `>Uncached`[T <: ~](that: DOT[T]): EXPONENTIAL[S, T] = {
            val exponentialDot = this.action.actionCarrier > that.action.actionCarrier
            val exp_x_G = exponentialDot.x(group.carrier)
            new ActionDot[S → T](
              group.action(exponentialDot) {
                case (f, g) =>
                  exponentialDot.transpose(exp_x_G) {
                    case (f ⊕ g, a) =>
                      that.action.actionMultiply(
                        exponentialDot.evaluate(
                          f,
                          dot.action.actionMultiply(
                            a,
                            group.inverse(g)
                          )
                        ),
                        g
                      )
                  }(
                    exp_x_G.pair(f, g)
                  )
              }
            ) with ExponentialDot[S, T] {
              override val source = dot
              override val target = that

              override def transpose[R <: ~](
                biArrow: BiArrow[R, S, T]
              ): R > (S → T) =
                ActionArrow[R, Ɛ.→[S, T]](
                  biArrow.product.left,
                  this,
                  exponentialDot.transpose(
                    biArrow.product.left.action.carrier
                  ) {
                    biArrow(_, _)
                  }
                )

              override def evaluate(
                function: S → T,
                arg: S
              ): T =
                exponentialDot.evaluate(
                  function,
                  arg
                )
            }
          }

          override def apply[T <: ~](
            target: DOT[T]
          )(
            f: S => T
          ): S > T =
            ActionArrow(
              this,
              target,
              action.actionCarrier(
                target.action.actionCarrier
              )(f)
            )

          override def sanityTest: Unit = {
            action.actionCarrier.sanityTest
            action.sanityTest
          }
        }

        object ActionDot {
          def apply[
            A <: ~
          ](
            actionCarrier: Ɛ.DOT[A]
          )(
            actionMultiply: (A, G) => A
          ) =
            new ActionDot(
              group.action(actionCarrier)(actionMultiply)
            )
        }

        case class ActionArrow[
          S <: ~,
          T <: ~
        ](
          override val source: ActionDot[S],
          override val target: ActionDot[T],
          arrow: Ɛ.>[S, T]
        ) extends Arrow[S, T] {
          override lazy val chi: T > TRUTH =
            ActionArrow(
              target,
              omega,
              arrow.chi
            )
          override def apply(s: S): T =
            arrow(s)
          override def ?=(that: S > T): EQUALIZER[S] = {
            val underlyingEqualizer = arrow ?= that.arrow
            new ActionDot(
              group.action(underlyingEqualizer) {
                source.action.actionMultiply
              }
            ) with EqualizingDot[S] { equalizer =>
              override val equalizerTarget = source
              override def restrict[R <: ~](anArrow: R > S) =
                ActionArrow(
                  anArrow.source,
                  equalizer,
                  underlyingEqualizer.restrict(anArrow.arrow)
                )
            }
          }

          override def o[R <: ~](that: R > S) =
            ActionArrow(
              that.source,
              target,
              arrow.o(that.arrow)
            )

          override def \[U <: ~](monic: U > T): S > U =
            ActionArrow(
              source,
              monic.source,
              arrow \ monic.arrow
            )

          override def sanityTest: Unit = {
            source.sanityTest
            target.sanityTest
            arrow.sanityTest
            assert(group.actions.isMorphism(source.action, target.action, arrow))
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
                group.action(
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

        override def functionAsArrow[
          S <: ~,
          T <: ~
        ](
          source: ActionDot[S],
          target: ActionDot[T],
          f: S => T
        ): S > T =
          source(target)(f)

        override def makeArrow[
          S <: ~,
          T <: ~
        ](
          prearrow: group.ActionPreArrow[S, T]
        ): ActionArrow[S, T] = {
          import prearrow.{function, source, target}
          new ActionArrow(
            makeDot(source),
            makeDot(target),
            source.actionCarrier(target.actionCarrier) {
              function
            }
          )
        }

        private val memoizedDotWrapper =
          Memoize.generic.withLowerBound[({ type λ[T <: ~] = group.Action[T] })#λ, ActionDot, ~] {
            new ActionDot(_)
          }

        override def makeDot[
          T <: ~
        ](
          predot: group.Action[T]
        ) =
          memoizedDotWrapper(predot)

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
        ): BiArrow[L, R, T] =
          left
            .x(right)
            .biArrow(
              target
            ) {
              bifunc
            }
      }
    }
  }
}
