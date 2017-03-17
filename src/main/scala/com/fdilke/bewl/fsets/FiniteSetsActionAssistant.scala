package com.fdilke.bewl.fsets

import FiniteSetsUtilities._
import FiniteSets.{~, >, Monoid, makeDot}
import scala.language.postfixOps

object FiniteSetsActionAssistant {
  def extractGenerators[
    M <: ~, 
    S <: ~
  ](
    monoid: Monoid[M]
  ) (
    action: monoid.Action[S]
  ): S > S = 
    ActionAnalyzer(
      monoid
    )(
      action
    ).extractGenerators
}