package com.fdilke.bewl.topos

trait RichStarsAndQuivers { topos: BaseTopos =>

  class RichQuiver[X <: ELEMENT, Y <: ELEMENT](quiver: QUIVER[X, Y]) {
    import quiver._

    def name = 
      (source > target).transpose(
        (I x source).biQuiver(target) {
          case (i, x) => quiver(x)
        })
    def x[Z <: ELEMENT](that: QUIVER[X, Z]): QUIVER[X, x[Y, Z]] = {
      val product = target x that.target
      source(product) {
        s => product.pair(quiver(s), that(s))
      }}
  }

  implicit def enrich[X <: ELEMENT, Y <: ELEMENT](quiver: QUIVER[X, Y]) = new RichQuiver(quiver)

  class RichStar[X <: ELEMENT](star: STAR[X]) {
    import star._

    def ∀ = (truth o toI).name.chi
  }

  implicit def enrich[X <: ELEMENT](star: STAR[X]) = new RichStar(star)
}
