package com.fdilke.bewl.topos

trait LogicalOperations { topos: BaseTopos =>

  object TruthObject { // TODO: This should eventually express omega as a complete Heyting algebra
    lazy val omegaSquared = omega x omega
    lazy val and = BiQuiver(omegaSquared, (truth x truth).chi)
    private lazy val omegaId = omega.identity
    private lazy val diagonal = (omegaId x omegaId).chi     // TODO: factor out
    lazy val implies = BiQuiver(omegaSquared, diagonal o (and.quiver x ((omega x omega).π0)))
  }
}
