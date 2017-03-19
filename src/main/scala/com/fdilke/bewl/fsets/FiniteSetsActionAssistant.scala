package com.fdilke.bewl.fsets

import FiniteSetsUtilities._
import FiniteSets.{~, >, Monoid, makeDot}
import scala.language.postfixOps
import scala.language.reflectiveCalls

object FiniteSetsActionAssistant {
  def generators[
    M <: ~, 
    S <: ~
  ](
    monoid: Monoid[M]
  ) (
    action: monoid.Action[S]
  ): S > S = 
    makeDot(
      FiniteSetsMonoidAnalyzer(
        monoid
      ).actionAnalyzer(
        action
      ).generators
    )(
      action.actionCarrier
    ) {
      identity[S]
    }
}