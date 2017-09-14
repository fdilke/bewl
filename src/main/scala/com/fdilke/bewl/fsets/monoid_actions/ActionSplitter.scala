package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.BaseFiniteSets
import com.fdilke.bewl.helper.BuildEquivalence

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

  trait ActionSplitter[ACTION[A]] {
    def splitAction[A](
      action: ACTION[A]
    ): ActionSplitting[A, ACTION]
  }

  object ActionSplitter {
    def forMonoid[M](
      monoid: Monoid[M]
    ): ActionSplitter[
      ({type λ[T] = monoid.Action[T]}) # λ
    ] =
      new ActionSplitter[
        ({type λ[T] = monoid.Action[T]}) # λ
      ] {
        private val monoidElements =
          monoid.carrier.elements

        private val findGenerators =
          FindGenerators forMonoid monoid

        override def splitAction[A](
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

          val blocks: Seq[Seq[Int]] =
            allGenerators.indices.groupBy(
              generatorSorts
            ).values.toSeq

          val handleBlock: Seq[Int] => ActionComponent[
            A,
            ({type λ[T] = monoid.Action[T]}) # λ
          ] = { block =>
            val componentGenerators: Seq[A] =
              block map allGenerators
            val component: monoid.Action[A] =
              monoid.action(
                makeDot(
                  for {
                    cg <- componentGenerators
                    m <- monoidElements
                  } yield
                    actionMultiply(cg, m)
                )
              )(
                actionMultiply
              )
            ActionComponent[
              A,
              ({type λ[T] = monoid.Action[T]})#λ
            ](
              componentGenerators,
              component
            )
          }

          ActionSplitting[
            A,
            ({type λ[T] = monoid.Action[T]}) # λ
          ] (
            allGenerators,
            blocks map handleBlock
          )
        }
      }
  }
}
