package com.fdilke.bewl.topos

trait LogicalOperations { topos: BaseTopos =>

  object TruthObject { // TODO: This should eventually express omega as a complete Heyting algebra
    lazy val and = BiQuiver(omega.squared, (truth x truth).chi)
    lazy val implies = BiQuiver(omega.squared, 
    	omega.=?=(
    		and.quiver, 
    		omega.squared.π0
    	))
    lazy val or = BiQuiver(omega.squared, 
    	omega.squared.forAll(omega) {
      		case ((a, b), w) => ((a > w) ^ (b > w)) > w
    })
  }
}
