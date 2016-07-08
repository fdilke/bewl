package com.fdilke.bewl.topos.enrichment

import com.fdilke.bewl.helper.⊕
import ⊕._
import com.fdilke.bewl.topos.{BaseTopos, ToposStructures}

trait LogicalOperations {

  Ɛ: BaseTopos with ToposStructures =>

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

    lazy val or =
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

  import TruthObject.falsity

  def isBoolean =
    (truth + falsity).isIso

  implicit class OmegaEnrichments(
    truthValue: TRUTH
  ) {
    import TruthObject._

    def →(that: TRUTH) = implies(truthValue, that)
    def ∧(that: TRUTH) = and(truthValue, that)
    def ∨(that: TRUTH) = or(truthValue, that)
  }
}
