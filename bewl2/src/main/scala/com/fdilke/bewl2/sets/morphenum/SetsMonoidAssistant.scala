package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.BaseSets
import com.fdilke.bewl2.helper.Memoize

import scala.Function.tupled
import scala.language.{postfixOps, reflectiveCalls}

trait SetsMonoidAssistant extends BaseSets:
  Ɛ: FindMonoidActionGenerators with FindMonoidActionPresentation with ActionSplitter =>

  override protected val monoidAssistant: MonoidAssistant = LocalMonoidAssistant

  object LocalMonoidAssistant extends MonoidAssistant:
    override def actionAnalyzer[M : Dot](monoid: Monoid[M]) : MonoidActionAnalyzer[monoid.Action, monoid.InternalMap] =
      new MonoidActionAnalyzer[monoid.Action, monoid.InternalMap]:
        // trait FarceActionAnalysis[ACTION[_], INTERNAL_MAP[_, _], A] extends 
        //   ActionAnalysis[A, ACTION, INTERNAL_MAP, [A] =>> FarceActionAnalysis[ACTION, INTERNAL_MAP, A]]:
        //   override def makeExponential[B](
        //     analysisB: FarceActionAnalysis[ACTION, INTERNAL_MAP, B]
        //   ): ACTION[INTERNAL_MAP[A, B]] =
        //     ???
        //   override def enumerateMorphisms[B](
        //     analysisB: FarceActionAnalysis[ACTION, INTERNAL_MAP, B]
        //   ): Iterable[A ~> B] =
        //     ???

        // override type ACTION_ANALYSIS[A] = FarceActionAnalysis[monoid.Action, monoid.InternalMap, A]
        // override def analyze[A](action: monoid.Action[A]): FarceActionAnalysis[monoid.Action, monoid.InternalMap, A] =
        //   ???

        override type ACTION_ANALYSIS[A] = SetsActionAnalysis[A]

        private val generatorFinder: MonoidActionGeneratorFinder[M, monoid.Action] =
          MonoidActionGeneratorFinder.forMonoid(monoid)

        private val presentationFinder: PresentationFinder[M, monoid.Action] =
          PresentationFinder.forMonoid(monoid, generatorFinder)

        private val actionSplitter: ActionSplitter[M, monoid.Action] =
          ActionSplitter.forMonoid(monoid, generatorFinder, presentationFinder)

        private val monoidElements: Set[M] =
          monoid.dot.dot

        override def analyze[A](action: monoid.Action[A]): SetsActionAnalysis[A] =
          new SetsActionAnalysis[A](action)

        override def makeExponential[A, B](
          analysisA: SetsActionAnalysis[A],
          analysisB: SetsActionAnalysis[B]
        ): monoid.Action[monoid.InternalMap[A, B]] = 
          analysisA.makeExponential(analysisB)

        override def enumerateMorphisms[A, B](
          src: SetsActionAnalysis[A],
          target: SetsActionAnalysis[B]
        ): Iterable[Map[A, B]] =
          src.enumerateMorphisms(target)

        class SetsActionAnalysis[A](val action: monoid.Action[A]):
          val actionSplitting: ActionSplitting[M, A, monoid.Action] =
            actionSplitter.splitAction(action)

          private val generatorsWithRelators: Seq[GeneratorWithRelators[M, A]] =
            presentationFinder(
              action,
              actionSplitting.allGenerators
            )

          private def mapsBetween[A, B](
            src: ActionComponent[M, A, monoid.Action],
            tgt: ActionComponent[M, B, monoid.Action]
          ): Iterable[Map[A, B]] =
            val srcGenerators: Seq[A] =
              src.componentGenerators
            val srcAction: monoid.Action[A] =
              src.componentAction
            val srcPresentation: Seq[GeneratorWithRelators[M, A]] =
              src.componentPresentation

            val targetAction: monoid.Action[B] = tgt.componentAction
            val targetCarrier: Dot[B] =
              targetAction.dot
            val targetElements: Set[B] =
              targetCarrier.dot
            val targetMultiply: ((B, M)) => B =
              targetAction.actionMultiply

            def compatibleExtensions(
              partialMap: Map[A, B],
              index: Int
            ): Iterable[Map[A, B]] =
              val gr = srcPresentation(index)
              val generator = gr.generator
              targetElements
                .filter { targetElement =>
                  gr.relators.forall { relator =>
                    val otherTarget: B =
                      if (relator.otherIndex == index)
                        targetElement
                      else
                        partialMap(
                          srcGenerators(relator.otherIndex)
                        )
                    targetMultiply(
                      targetElement,
                      relator.selfScalar
                    ) == targetMultiply(
                      otherTarget,
                      relator.otherScalar
                    )
                  }
                }
                .map { targetElement => {
                    for
                      m <- monoidElements
                    yield 
                      srcAction.actionMultiply(
                        generator,
                        m
                      ) -> targetMultiply(
                        targetElement,
                        m
                      )
                  } toMap
                }
                .toSeq distinct

            srcPresentation.indices.foldLeft(
              Iterable(Map.empty[A, B])
            ) { (partialMaps, index) =>
              for
                partialMap <- partialMaps
                ext <- compatibleExtensions(partialMap, index)
              yield 
                partialMap ++ ext
            }            

          def enumerateMorphisms[B](
            target: SetsActionAnalysis[B]
          ): Iterable[Map[A, B]] =
            val srcComponents: Seq[ActionComponent[M, A, monoid.Action]] =
              actionSplitting.components
            val tgtComponents: Seq[ActionComponent[M, B, monoid.Action]] =
              target.actionSplitting.components
            val targetAction: monoid.Action[B] = 
              target.action

            val blockMaps =
              Memoize[
                (Int, Int),
                Iterable[
                  Map[A, B]
                ]
              ] {
                tupled { (i, j) =>
                  mapsBetween(
                    srcComponents(i),
                    tgtComponents(j)
                  )
                }
              }

            def absorb(
              partialMaps: Iterable[Map[A, B]],
              srcIndex: Int
            ): Iterable[Map[A, B]] =
              for
                partialMap <- partialMaps
                tgtIndex <- tgtComponents.indices
                continuation <- blockMaps(
                  srcIndex,
                  tgtIndex
                )
              yield 
                partialMap ++ continuation

            srcComponents.indices
              .foldLeft(
                Iterable(
                  Map.empty[A, B]
                )
              ) ( absorb )

          given Dot[A] = action.dot
          given Dot[M] = monoid.dot
          lazy val recursiveImago: SetsActionAnalysis[(M, A)] =
            analyze(
              monoid.withRegularAction(
                (regularAction: monoid.Action[M]) ?=>
                  regularAction.x(action)
                )
            )

          // TODO: should be able to optimize away
          private def mapToArrow[A, B](
            arrow: Map[(M, A), B]
          ): (M, A) => B =
            (m, a) => arrow(m -> a)

          def makeExponential[B](
            analysisB: SetsActionAnalysis[B]
          ): monoid.Action[monoid.InternalMap[A, B]] = 
            val mXa: Set[(M, A)] =
              summon[Dot[(M, A)]].dot
            // TODO: should be able to optimize away
            def arrowToMap(
              arrow: ((M, A)) => B
            ): Map[(M, A), B] =
              mXa.map(m_a => m_a -> arrow(m_a)) toMap

            val morphisms: Set[Map[(M, A), B]] =
                recursiveImago
                  .enumerateMorphisms(
                    analysisB
                  ).map(arrowToMap).toSet

            monoid.hackTag[monoid.Action, A, B](
              withDot(morphisms) {
                monoid.Action[Map[(M, A), B]] { (f, m) =>
                  arrowToMap(
                    (n, a) => f(monoid.multiply(m, n) -> a)
                  )
                }
              }
            )

