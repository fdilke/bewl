package com.fdilke.bewl.topos

trait LogicalOperations { topos: BaseTopos =>

  object TruthObject { // TODO: This should eventually express omega as a complete Heyting algebra
    lazy val and = BiArrow(omega.squared, (truth x truth).chi)
    lazy val implies = BiArrow(omega.squared, 
    	omega.=?=(
    		and.arrow, 
    		omega.squared.π0
    	))
    lazy val or = BiArrow(omega.squared, 
    	omega.squared.forAll(omega) {
      		case ((a, b), ω) => ((a > ω) ^ (b > ω)) > ω
    })
    lazy val falsity = I.forAll(omega) {
      (_, ω) => ω
    }
  }

  object Arrow {
    def fromFunctionalRelation[A <: ~, B <: ~](
      source: DOT[A],
      target: DOT[B]
    )(
      predicate: (A, B) => TRUTH
    ): ARROW[A, B] = {
      val product = source x target
      val graph = product(omega) {
        case (a, b) => predicate(a, b)
      }.whereTrue.inclusion
      (product.π1 o graph) / (product.π0 o graph)
    }
  }
}
