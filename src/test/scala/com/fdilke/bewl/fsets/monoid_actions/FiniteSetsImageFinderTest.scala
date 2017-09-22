package com.fdilke.bewl.fsets.monoid_actions

import com.fdilke.bewl.fsets.FiniteSets
import com.fdilke.bewl.fsets.FiniteSets.{>, LocalMonoidAssistant, ToposOfMonoidActions, bifunctionAsBiArrow, functionAsArrow}
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.helper.⊕
import com.fdilke.bewl.testutil.CustomMatchers._
import com.fdilke.bewl.topos.algebra.KnownMonoids.monoidOf3
import org.scalatest.FreeSpec
import org.scalatest.Matchers._

import scala.Function.untupled
import scala.language.{existentials, higherKinds, postfixOps, reflectiveCalls}

class FiniteSetsImageFinderTest extends FreeSpec {
  
  "The local image finder" - {
    "gives the same results as the default one" in {
      val source = dot(1,2,3)
      val target = dot('A, 'B, 'C, 'D)
      for {
        s2t <- source >> target
      } {
        val defaultImage =
          FiniteSets.DefaultImageFinder.image(s2t)
        val localImage =
          FiniteSets.imageFinder.image(s2t)

        localImage.equalizerTarget shouldBe
          defaultImage.equalizerTarget

        elementsOf(localImage).toSet shouldBe
          elementsOf(defaultImage).toSet

        localImage.restrict(
          defaultImage.inclusion
        ) shouldBe 'iso

        defaultImage.restrict(
          localImage.inclusion
        ) shouldBe 'iso
      }
    }
  }
}
