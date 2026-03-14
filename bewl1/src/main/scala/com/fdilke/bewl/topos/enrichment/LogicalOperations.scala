package com.fdilke.bewl.topos.enrichment

import com.fdilke.bewl.helper.{⊕, Timed}
import com.fdilke.bewl.topos.algebra.AlgebraicStructures
import com.fdilke.bewl.topos.{BaseTopos, ToposAlgebra, ToposStructures}

trait LogicalOperations {

  Ɛ: BaseTopos with ToposStructures with ToposAlgebra =>

  trait LogicalOperations {
    val and: BiArrow[TRUTH, TRUTH, TRUTH]
    val implies: BiArrow[TRUTH, TRUTH, TRUTH]
    val or: BiArrow[TRUTH, TRUTH, TRUTH]
    val falsity: UNIT > TRUTH
  }

  class DefaultLogicalOperations extends LogicalOperations {
    override lazy val and =
      BiArrow(
        omega.squared,
        truth.x(truth).chi
      )

    override lazy val implies =
      BiArrow(
        omega.squared,
        omega.=?=(
          and.arrow,
          omega.squared.π0
        )
      )

    override lazy val or: BiArrow[TRUTH, TRUTH, TRUTH] =
      BiArrow(
        omega.squared,
        omega.squared.forAll(omega) {
          case (a ⊕ b, ω) =>
            (a → ω ∧ (b → ω)) → ω
        }
      )

    override lazy val falsity: UNIT > TRUTH =
      I.forAll(omega)((_, ω) => ω)
  }

  val logicalOperations: LogicalOperations =
    new DefaultLogicalOperations

  lazy val Ω: HeytingAlgebra[TRUTH] =
    new HeytingAlgebra[TRUTH](
      omega,
      logicalOperations.falsity,
      truth,
      logicalOperations.and,
      logicalOperations.or,
      logicalOperations.implies
    )

  lazy val falsity: UNIT > TRUTH =
    logicalOperations.falsity

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
