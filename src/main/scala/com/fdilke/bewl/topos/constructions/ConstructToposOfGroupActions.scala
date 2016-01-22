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
          ???
        override val omega: DOT[TRUTH] =
          ???
        override val truth: >[UNIT, TRUTH] =
          ???

        override type WRAPPER[T <: ~] = T

        class ActionDot[
          S <: ~
        ] extends Dot[S] {
          override val toI: S > UNIT =
            ???
          override val globals: Traversable[UNIT > S] =
            ???
          override def xUncached[T <: ~](that: DOT[T]): BIPRODUCT[S, T] =
            ???
          override def `>Uncached`[T <: ~](that: DOT[T]): EXPONENTIAL[S, T] =
            ???
          override def apply[T <: ~](target: DOT[T])(f: S => T) : S > T =
            ???
          override def sanityTest =
            ???
        }

        class ActionArrow[
          S <: ~,
          T <: ~
        ](
          override val source: DOT[S],
          override val target: DOT[T],
          override val chi: T > TRUTH
        ) extends Arrow[S, T] {
          override def apply(s: S): T =
            ???
          override def ?=(that: S > T): EQUALIZER[S] =
            ???
          override def o[R <: ~](that: R > S) : R > T =
            ???
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
          ???

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
        ): ActionDot[T] =
          ???

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
        ): BiArrow[WRAPPER[L], WRAPPER[R], WRAPPER[T]] =
          ???
      }
      new TheTopos
    }
  }
}
