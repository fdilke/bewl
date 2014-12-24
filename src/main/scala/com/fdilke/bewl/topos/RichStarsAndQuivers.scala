package com.fdilke.bewl.topos

trait RichStarsAndQuivers { topos: BaseTopos =>

  implicit class RichQuiver[X <: ELEMENT, Y <: ELEMENT](quiver: QUIVER[X, Y]) {
    import quiver._

    def name = 
      (source > target).transpose(
        (I x source).biQuiver(target) {
          case (i, x) => quiver(x)
        })
    def x[Z <: ELEMENT](that: QUIVER[X, Z]): QUIVER[X, Y x Z] = {
      val product = target x that.target
      source(product) {
        s => product.pair(quiver(s), that(s))
      }}
  }

  // TODO: make this a trait always included, ∀ and ∃ lazy val

//  private def ∃[X <: ELEMENT](star: STAR[X]) =

  object TruthObject { // TODO: This should eventually express omega as a complete Heyting algebra
    lazy val omegaSquared = omega x omega
    lazy val and = BiQuiver(omegaSquared, (truth x truth).chi)
    private lazy val omegaId = omega.identity
    private lazy val diagonal = (omegaId x omegaId).chi     // TODO: factor out
    lazy val implies = BiQuiver(omegaSquared, diagonal o (and.quiver x ((omega x omega).π0)))
  }
}
