package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.BaseFiniteSets
import com.fdilke.bewl.helper.⊕
import com.fdilke.bewl.helper.⊕._

import scala.Function.tupled
import scala.language.{higherKinds, postfixOps, reflectiveCalls}

trait ActionSplitter extends BaseFiniteSets {

  object ActionSplitter {
    def forMonoid[M](
      monoid: Monoid[M]
    ): {
      def splitAction[A](
        action: monoid.Action[A]
      ): Seq[monoid.Action[A]]
    } =
      new Object {
        private val monoidElements =
          monoid.carrier.elements

        def splitAction[A](
          action: monoid.Action[A]
        ): Seq[monoid.Action[A]] =
          ???
      }
  }
}
