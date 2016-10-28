package com.fdilke.bewl.topos.enrichment

import com.fdilke.bewl.helper.⊕
import com.fdilke.bewl.topos.algebra.AlgebraicStructures
import com.fdilke.bewl.topos.{BaseTopos, ToposStructures}

trait LogicalOperations {

  Ɛ: BaseTopos with
    ToposStructures with
    AlgebraicStructures =>

  object TruthObject {
    // TODO: This should eventually express omega as a
    // complete Heyting algebra

    lazy val and =
      BiArrow(
        omega.squared,
        (truth x truth).chi
      )

    lazy val implies =
      BiArrow(
        omega.squared,
        omega.=?=(
          and.arrow,
          omega.squared.π0
        )
      )

    lazy val or: BiArrow[TRUTH, TRUTH, TRUTH] =
      BiArrow(
        omega.squared,
        omega.squared.forAll(omega) {
            case (a ⊕ b, ω) => ((a → ω) ∧ (b → ω)) → ω
        }
      )
    lazy val falsity =
      I.forAll(omega) {
        (_, ω) => ω
      }
  }

  lazy val Ω =
    new HeytingAlgebra[TRUTH](
      omega,
      TruthObject.falsity,
      truth,
      TruthObject.and,
      TruthObject.or,
      TruthObject.implies
    )

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
