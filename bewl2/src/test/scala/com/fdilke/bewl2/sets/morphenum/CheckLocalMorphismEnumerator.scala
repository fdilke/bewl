package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.Sets
import Sets.{ Dot, DefaultMonoidAssistant, LocalMonoidAssistant, Monoid, MonoidActionAnalyzer, 
  Group, LocalGroupAssistant, DefaultGroupAssistant, GroupActionAnalyzer, RichArrow }
import com.fdilke.bewl2.utility.RichFunSuite._

object CheckLocalMorphismEnumerator:
  def apply[M, X, Y](monoid: Monoid[M])(
    sourceAction: monoid.Action[X],
    targetAction: monoid.Action[Y],
    thorough: Boolean
  ): Unit =
    given Dot[M] = monoid.dot
    given Dot[X] = sourceAction.dot
    given monoid.Action[X] = sourceAction
    given Dot[Y] = targetAction.dot
    given monoid.Action[Y] = targetAction
    val localAnalyzer: MonoidActionAnalyzer[monoid.Action, monoid.InternalMap] =
      LocalMonoidAssistant.actionAnalyzer(monoid)

    def enumerateMorphisms(
      analyzer: MonoidActionAnalyzer[monoid.Action, monoid.InternalMap]
    ): Iterable[X => Y] =
      analyzer.enumerateMorphisms(
          analyzer.analyze(sourceAction),
          analyzer.analyze(targetAction)
        )

    val localMorphisms: Iterable[X => Y] =
      enumerateMorphisms(localAnalyzer)

    localMorphisms.forall {
      monoid.actions.isMorphism(_)
    } is true

    if (thorough) {
      val defaultAnalyzer: MonoidActionAnalyzer[monoid.Action, monoid.InternalMap] =
        DefaultMonoidAssistant.actionAnalyzer(monoid)

      val defaultMorphisms: Iterable[X => Y] =
        enumerateMorphisms(defaultAnalyzer)

      checkNoDuplicates(localMorphisms)
      checkSameMorphisms(localMorphisms, defaultMorphisms)
    }

  def apply[G, X, Y](group: Group[G])(
    sourceAction: group.Action[X],
    targetAction: group.Action[Y],
    thorough: Boolean
  ): Unit =
    given Dot[G] = group.dot
    given Dot[X] = sourceAction.dot
    given group.Action[X] = sourceAction
    given Dot[Y] = targetAction.dot
    given group.Action[Y] = targetAction
    val localAnalyzer: GroupActionAnalyzer[group.Action] =
      LocalGroupAssistant.actionAnalyzer(group)

    def enumerateMorphisms(
      analyzer: GroupActionAnalyzer[group.Action]
    ): Iterable[X => Y] =
      analyzer.enumerateMorphisms(
          analyzer.analyze(sourceAction),
          analyzer.analyze(targetAction)
        )

    val localMorphisms: Iterable[X => Y] =
      enumerateMorphisms(localAnalyzer)

    localMorphisms.forall {
      group.actions.isMorphism(_)
    } is true

    if (thorough) {
      val defaultAnalyzer: GroupActionAnalyzer[group.Action] =
        DefaultGroupAssistant.actionAnalyzer(group)

      val defaultMorphisms: Iterable[X => Y] =
        enumerateMorphisms(defaultAnalyzer)

      checkNoDuplicates(localMorphisms)
      checkSameMorphisms(localMorphisms, defaultMorphisms)
    }

  def checkNoDuplicates[X: Dot, Y: Dot](
    morphisms: Iterable[X => Y]
  ): Unit =
    for
      (f, i) <- morphisms.zipWithIndex
      (g, j) <- morphisms.zipWithIndex if (i < j)
    do
      (f =!= g) is false

  private def asMap[X: Dot, Y: Dot](
    morphism: X => Y
  ): Map[X, Y] =
    summon[Dot[X]].dot.map { x => 
      x -> morphism(x)
    }.toMap

  private def asMaps[X: Dot, Y: Dot](
    morphisms: Iterable[X => Y]
  ): Set[Map[X, Y]] =
    morphisms.map(asMap).toSet

  def checkSameMorphisms[X: Dot, Y: Dot](
    morphisms1: Iterable[X => Y],
    morphisms2: Iterable[X => Y]
  ): Unit =
    asMaps(morphisms1) is asMaps(morphisms2)

