package com.fdilke.bewl.topos.enrichment

import com.fdilke.bewl.helper.⊕
import com.fdilke.bewl.topos.algebra.AlgebraicStructures
import com.fdilke.bewl.topos.{BaseTopos, ToposStructures}

trait LogicalOperations {

  Ɛ: BaseTopos with
    ToposStructures with
    AlgebraicStructures =>

  lazy val Ω: HeytingAlgebra[TRUTH] = {
    val and =
      BiArrow(
        omega.squared,
        (truth x truth).chi
      )

    val implies =
      BiArrow(
        omega.squared,
        omega.=?=(
          and.arrow,
          omega.squared.π0
        )
      )

    val or: BiArrow[TRUTH, TRUTH, TRUTH] =
      BiArrow(
        omega.squared,
        omega.squared.forAll(omega) {
          case (a ⊕ b, ω) => ((a → ω) ∧ (b → ω)) → ω
        }
      )
    val falsity =
      I.forAll(omega) {
        (_, ω) => ω
      }

    new HeytingAlgebra[TRUTH](
      omega,
      falsity,
      truth,
      and,
      or,
      implies
    )
  }

  lazy val falsity: UNIT > TRUTH =
    Ω.bottom

  def isBoolean =
    (truth + Ω.bottom).isIso

  implicit class OmegaEnrichments(
    truthValue: TRUTH
  ) {
    def →(that: TRUTH) =  // implies(truthValue, that)
      Ω.implies(truthValue, that)

    def ∧(that: TRUTH) = // and(truthValue, that)
      Ω.meet(truthValue, that)

    def ∨(that: TRUTH) = // or(truthValue, that)
      Ω.join(truthValue, that)
  }
}
