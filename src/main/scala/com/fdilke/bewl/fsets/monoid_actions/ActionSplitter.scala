package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.{BaseFiniteSets, FiniteSets}
import com.fdilke.bewl.helper.{BuildEquivalence, ⊕}
import com.fdilke.bewl.helper.⊕._

import scala.Function.tupled
import scala.collection.immutable
import scala.language.{higherKinds, postfixOps, reflectiveCalls}

trait ActionSplitter extends BaseFiniteSets {
  Ɛ: FindGenerators =>

  case class ActionComponent[
    A,
    ACTION[B]
  ](
    componentGenerators: Seq[A],
    componentAction: ACTION[A]
  )

  case class ActionSplitting[
    A,
    ACTION[B]
  ] (
    allGenerators: Seq[A],
    components: Seq[
      ActionComponent[A, ACTION]
    ]
  )

  object ActionSplitter {
    def forMonoid[M](
      monoid: Monoid[M]
    ): {
      def splitAction[A](
        action: monoid.Action[A]
      ): ActionSplitting[
          A,
          ({type λ[T] = monoid.Action[T]}) # λ
        ]
    } =
      new Object {
        private val monoidElements =
          monoid.carrier.elements

        private val findGenerators =
          FindGenerators forMonoid monoid

        def splitAction[A](
          action: monoid.Action[A]
        ) = {
          val allGenerators: Seq[A] =
            findGenerators apply action generators
          val indexedGenerators: Seq[(A, Int)] =
            allGenerators.zipWithIndex
          val actionMultiply: BiArrow[A, M, A] =
            action.actionMultiply
          val generatorSorts: Seq[Int] =
            BuildEquivalence(
              allGenerators.size,
              for {
                (g, i) <- indexedGenerators
                (h, j) <- indexedGenerators
                m <- monoidElements
                n <- monoidElements
                if actionMultiply(g, m) ==
                  actionMultiply(h, n)
              }
                yield i -> j
            )

          new ActionSplitting[
            A,
            ({type λ[T] = monoid.Action[T]}) # λ
          ] (
            allGenerators,
            allGenerators.indices.groupBy(
              generatorSorts
            ).values map { (block: Seq[Int]) =>
              {
                val componentGenerators: Seq[A] =
                  block map allGenerators
                ActionComponent[
                  A,
                  ({type λ[T] = monoid.Action[T]}) # λ
                  ] (
                  componentGenerators,
                  monoid.action(
                    makeDot(
                      for {
                        cg <- componentGenerators
                        m <- monoidElements
                      } yield
                        actionMultiply(cg, m)
                    )
                  ) (
                    actionMultiply
                  )
                )
              }
            } toSeq
          )
        }
      }
  }
}
