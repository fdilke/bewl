package com.fdilke.bewl.topos.constructions

import com.fdilke.bewl.topos.algebra.{AlgebraicMachinery, AlgebraicStructures}
import com.fdilke.bewl.topos.{Topos, Wrappings, BaseTopos, LogicalOperations}

trait ConstructToposOfGroupActions extends BaseTopos with LogicalOperations {
  Ɛ: AlgebraicStructures with AlgebraicMachinery =>

  object ToposOfGroupActions {
    def of[G <: ~](group: Ɛ.Group[G]) : Topos with Wrappings[
      Ɛ.~,
      ({type λ[X <: Ɛ.~] = group.Action[X]})#λ,
      ({type λ[X <: Ɛ.~, Y <: Ɛ.~] = group.ActionPreArrow[X, Y]})#λ
    ] = {
      import group.{carrier, multiply, unit, Action, ActionPreArrow}
      class TheTopos extends Topos with Wrappings[
        Ɛ.~,
        ({type λ[X <: Ɛ.~] = group.Action[X]})#λ,
        ({type λ[X <: Ɛ.~, Y <: Ɛ.~] = group.ActionPreArrow[X, Y]})#λ
      ] {
        override type ~ = Ɛ.~
        override type DOT[X <: ~] = ActionDot[X]
        override type >[X <: ~, Y <: ~] = ActionArrow[X, Y]
        override type UNIT = Ɛ.UNIT
        override type TRUTH = Ɛ.TRUTH

        override val I: DOT[UNIT] =
          ActionDot(Ɛ.I) { (i, m) => i }

        override lazy val omega: DOT[TRUTH] =
          ???

        override lazy val truth: >[UNIT, TRUTH] =
          ???

        override type WRAPPER[T <: ~] = T

        class ActionDot[
          S <: ~
        ] (
          private val action: group.Action[S]
        ) extends Dot[S] { dot =>

          override val toI: S > UNIT =
            new ActionArrow(this, I, action.actionCarrier.toI)

          override lazy val globals: Traversable[UNIT > S] =
            ???

          override def xUncached[T <: ~](that: DOT[T]): BIPRODUCT[S, T] = {
            val productDot = this.action.actionCarrier x that.action.actionCarrier
            val productMultiply: ((S x T), G) => (S x T) = {
              case ((s, t), g) =>
                productDot.pair(
                  this.action.actionMultiply(s, g),
                  that.action.actionMultiply(t, g)
                )
            }
            val productAction = group.action(productDot)(productMultiply)
            new ActionDot[S x T](productAction) with BiproductDot[S, T, S x T] {
              override val left = dot
              override val right = that
              override def pair(s: S, t: T) =
                productDot.pair(s, t)
            }
          }

          override def `>Uncached`[T <: ~](that: DOT[T]): EXPONENTIAL[S, T] =
            ???

          override def apply[T <: ~](target: DOT[T])(f: S => T) : S > T =
            new ActionArrow(
              this,
              target,
              action.actionCarrier(
                target.action.actionCarrier
              ) { f }
            )

          override def sanityTest =
            ???
        }

        object ActionDot {
          def apply[
            A <: Ɛ.~
          ] (
             actionCarrier: Ɛ.DOT[A]
          ) (
             actionMultiply: (A, G) => A
          ) =
            new ActionDot(
              group.action(actionCarrier)(actionMultiply)
            )
        }

        class ActionArrow[
          S <: ~,
          T <: ~
        ](
          override val source: DOT[S],
          override val target: DOT[T],
          private val arrow: Ɛ.>[S, T]
        ) extends Arrow[S, T] {
          override lazy val chi: T > TRUTH =
            ???
          override def apply(s: S): T =
            ???
          override def ?=(that: S > T): EQUALIZER[S] =
            ???
          override def o[R <: ~](that: R > S) =
            new ActionArrow(
              that.source,
              target,
              arrow o that.arrow
            )

          override def \[U <: ~](monic: U > T) : S > U =
            ???
          override def sanityTest =
            ???
        }

        override def functionAsArrow[
          S <: ~,
          T <: ~
        ](
          source: ActionDot[S],
          target: ActionDot[T],
          f: S => T
        ): S > T =
          source(target) { f }

        override def makeArrow[
          S <: ~,
          T <: ~
        ] (
          prearrow: group.ActionPreArrow[S, T]
        ): ActionArrow[S, T] =
          ???

        override def makeDot[
          T <: ~
        ] (
          predot: group.Action[T]
        ) =
          new ActionDot(predot)

        override def bifunctionAsBiArrow[
          L <: ~,
          R <: ~,
          T <: ~
        ] (
          left: ActionDot[L],
          right: ActionDot[R],
          target: ActionDot[T]
        ) (
          bifunc: (L, R) => T
        ): BiArrow[L, R, T] =
          (left x right).biArrow(
            target
          ) {
            bifunc
          }
      }
      new TheTopos
    }
  }
}
