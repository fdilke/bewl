package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.fsets.FiniteSets.{makeDot, Monoid, >}
import scala.language.postfixOps
import scala.language.reflectiveCalls

object FiniteSetsActionAssistant {
  def generators[
    M, 
    S
  ](
    monoid: Monoid[M]
  ) (
    action: monoid.Action[S]
  ): S > S = 
    makeDot(
      FiniteSetsMonoidAction(
        monoid
      ).analyze (
        action
      ).generators
    )(
      action.actionCarrier
    ) {
      identity[S]
    }
}