package com.fdilke.bewl.diagrammatic.algebra

import com.fdilke.bewl.diagrammatic.BaseDiagrammaticTopos

trait TruthObject { topos: BaseDiagrammaticTopos with DiagrammaticAlgebra with DiagrammaticAlgebraicLaws with DiagrammaticAlgebraicStructures =>

  // TODO: do these need to be lazy? why?
  private lazy val and: ARROW[(OMEGA, OMEGA), OMEGA] = (truth x truth).chi.arrow
  private lazy val omegaId = omega.identity
  private lazy val diagonal = (omegaId x omegaId).chi.arrow     // TODO: factor out
  private lazy val po20: ARROW[(OMEGA, OMEGA), OMEGA] = projection(omega, 2, 0).asInstanceOf[ARROW[(OMEGA, OMEGA), OMEGA]]
  private lazy val implies = diagonal(and x po20)

  def all[X](x: DOT[X]): ARROW[X => OMEGA, OMEGA] = truth(x.toI).name.chi.arrow
  def exists[X](x: DOT[X]): ARROW[X => OMEGA, OMEGA] = {
    val p = omega ^ x
    val pox: DOT[((X => OMEGA, OMEGA), X)] = p x omega x x

    val lhs: ARROW[((X => OMEGA, OMEGA), X), OMEGA] = evaluation(x, omega)(leftProjection(p, omega, x), rightProjection(p, omega, x))
    val rhs: ARROW[((X => OMEGA, OMEGA), X), OMEGA] = midProjection(p, omega, x)
    val fx_implies_w: ARROW[((X => OMEGA, OMEGA), X), OMEGA] = implies(lhs x rhs)
    val forall_x_fx_implies_w : ARROW[(X => OMEGA, OMEGA), OMEGA] = all(x)(transpose(x, omega, BiArrow(p x omega, x, fx_implies_w)))
    val bigImplies: ARROW[(X => OMEGA, OMEGA), OMEGA] = implies(forall_x_fx_implies_w x rightProjection(p, omega))
    all(omega)(transpose(omega, omega, BiArrow(p, omega, bigImplies)))
  }
}
