package com.fdilke.bewl.fsets

import scala.language.{postfixOps, reflectiveCalls}

trait FiniteSetsImageFinder extends BaseFiniteSets {
  override val imageFinder: ImageFinder =
    new ImageFinder {
      def image[
        S <: ~,
        T <: ~
      ](
        arrow: S > T
      ): EQUALIZER[T] =
        new FiniteSetsDot[T](
          arrow.source.elements.map(arrow.function).toSeq.distinct
        ) with EqualizingDot[T] { equalizer =>

          override val equalizerTarget: DOT[T] =
            arrow.target

          override def restrict[
            R
          ](subdot: R > T): R > T =
            subdot.source(equalizer) {
              subdot(_)
            }
        }
    }
}
