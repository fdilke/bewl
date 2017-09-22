package com.fdilke.bewl.fsets

import com.fdilke.bewl.fsets.monoid_actions.{ActionSplitter, FindGenerators, FindPresentation, GeneratorWithRelators}
import com.fdilke.bewl.helper.Memoize

import scala.Function.tupled
import scala.language.{higherKinds, postfixOps, reflectiveCalls}

trait FiniteSetsImageFinder extends BaseFiniteSets {
  override val imageFinder: ImageFinder =
    DefaultImageFinder

  object DefaultImageFinder extends ImageFinder {
      def image[
        S <: ~,
        T <: ~
      ](
         arrow: S > T
       ): EQUALIZER[T] = {
        new FiniteSetsDot[T](
          arrow.source.elements map arrow.function
        ) with EqualizingDot[T] { equalizer =>

          override val equalizerTarget: DOT[T] =
            arrow.target

          override def restrict[
            R
          ] (subdot: R > T): R > T =
            subdot.source(equalizer) {
              subdot(_)
            }
        }
      }
    }
}

