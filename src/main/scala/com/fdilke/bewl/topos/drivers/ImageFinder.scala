package com.fdilke.bewl.topos.drivers

import com.fdilke.bewl.topos._

import scala.language.postfixOps

trait ImageFinder extends BaseTopos with ToposEnrichments {

  Ɛ: ToposPrerequisites =>

  trait ImageFinder {
    def image[
      S <: ~,
      T <: ~
    ](
      arrow: S > T
    ): EQUALIZER[T]
  }

  val imageFinder: ImageFinder =
    DefaultImageFinder

  object DefaultImageFinder extends ImageFinder {
    def image[
      S <: ~,
      T <: ~
    ](
      arrow: S > T
    ): EQUALIZER[T] =
      arrow.target
        .existsViaE(
          arrow.source
        ) { (t, s) =>
          arrow.target.=?=(
            arrow(s),
            t
          )
      } whereTrue
  }
}
