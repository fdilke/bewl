package com.fdilke.bewl2.sets.morphenum

import com.fdilke.bewl2.sets.BaseSets

trait SetsGroupAssistant extends BaseSets:
  Ɛ: FindGroupGenerators =>

  override protected val groupAssistant: GroupAssistant = LocalGroupAssistant

  object LocalGroupAssistant extends GroupAssistant:
    override def actionAnalyzer[G : Dot](group: Group[G]) : GroupActionAnalyzer[group.Action] =
      ???
