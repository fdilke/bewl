package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.fsets.FiniteSets.{makeDot, Monoid, >}
import scala.language.postfixOps
import scala.language.reflectiveCalls

object FiniteSetsActionAssistant {
  // May be resurrected in some form later (as an implementation of some
  // trait which computes action exponentials and does fast arrow comparisons)
//  def findGenerators[
//    M, 
//    S
//  ](
//    monoid: Monoid[M]
//  ) (
//    action: monoid.Action[S]
//  ): S > S = 
//    makeDot(
//      FiniteSetsMonoidAction(
//        monoid
//      ).analyze (
//        action
//      ).generators
//    )(
//      action.actionCarrier
//    ) {
//      identity[S]
//    }
//
//  def findGeneratorsWithRelators[
//    M, 
//    S
//  ](
//    monoid: Monoid[M]
//  ) (
//    action: monoid.Action[S]
//  ): Seq[GeneratorWithRelators[M, S]] = 
//      FiniteSetsMonoidAction(
//        monoid
//      ).analyze (
//        action
//      ).presentation
}