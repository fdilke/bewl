package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.BaseSets
import com.fdilke.bewl2.helper.BuildEquivalence

import scala.language.{postfixOps, reflectiveCalls}

trait ActionSplitter extends BaseSets {
  Ɛ: FindGenerators with FindPresentation =>

  case class ActionComponent[
    M,
    A,
    ACTION[B]
  ](
    componentGenerators: Seq[A],
    componentAction: ACTION[A],
    componentPresentation: Seq[
      GeneratorWithRelators[M, A]
    ]
  )

  case class ActionSplitting[
    M,
    A,
    ACTION[B]
  ](
    allGenerators: Seq[A],
    components: Seq[
      ActionComponent[M, A, ACTION]
    ]
  )

  trait ActionSplitter[M, ACTION[A]] {
    def splitAction[A](
      action: ACTION[A]
    ): ActionSplitting[M, A, ACTION]
  }

  object ActionSplitter {
    def forMonoid[M](
      monoid: Monoid[M]
    ): ActionSplitter[
      M,
      ({ type λ[T] = monoid.Action[T] })#λ
    ] =
      new ActionSplitter[
        M,
        ({ type λ[T] = monoid.Action[T] })#λ
      ] {
        private val monoidElements: Set[M] =
          monoid.dot.dot

        private val generatorFinder: GeneratorFinder[M, monoid.Action] =
          FindGenerators.forMonoid(monoid)

        private val presentationFinder: PresentationFinder[M, monoid.Action] =
          PresentationFinder.forMonoid(monoid, generatorFinder)

        override def splitAction[A](
          action: monoid.Action[A]
        ) = {
          val allGenerators: Seq[A] =
            generatorFinder.findGenerators(action) generators
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
              } yield i -> j
            )

          val blocks: Seq[Seq[Int]] =
            allGenerators.indices
              .groupBy(
                generatorSorts
              )
              .values
              .toSeq

          val handleBlock: Seq[Int] => ActionComponent[
            M,
            A,
            ({ type λ[T] = monoid.Action[T] })#λ
          ] = { block =>
            val componentGenerators: Seq[A] =
              block.map(allGenerators)
            val componentSet: Set[A] = {
              for {
                    cg <- componentGenerators
                    m <- monoidElements
                  } yield actionMultiply(cg, m)
                }.toSet
            val componentAction: monoid.Action[A] =
              withDot(componentSet) {
                monoid.Action { actionMultiply }
              }
            val componentPresentation: Seq[GeneratorWithRelators[M, A]] =
              presentationFinder.findPresentation(
                componentAction,
                componentGenerators
              )
            ActionComponent[
              M,
              A,
              ({ type λ[T] = monoid.Action[T] })#λ
            ](
              componentGenerators,
              componentAction,
              componentPresentation
            )
          }

          ActionSplitting[
            M,
            A,
            ({ type λ[T] = monoid.Action[T] })#λ
          ](
            allGenerators,
            blocks.map(handleBlock)
          )
        }
      }
  }
}
