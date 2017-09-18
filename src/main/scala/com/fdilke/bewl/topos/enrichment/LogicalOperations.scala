package com.fdilke.bewl.topos.enrichment

import com.fdilke.bewl.helper.⊕
import com.fdilke.bewl.topos.algebra.AlgebraicStructures
import com.fdilke.bewl.topos.{BaseTopos, ToposAlgebra, ToposStructures}

trait LogicalOperations {

  Ɛ: BaseTopos with
    ToposStructures with
    ToposAlgebra =>

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
    val falsity: UNIT > TRUTH =
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
    (truth + falsity).isIso

  implicit class OmegaEnrichments(
    truthValue: TRUTH
  ) {
    def →(that: TRUTH) =
      Ω.implies(truthValue, that)

    def ∧(that: TRUTH) =
      Ω.meet(truthValue, that)

    def ∨(that: TRUTH) =
      Ω.join(truthValue, that)
  }
}
