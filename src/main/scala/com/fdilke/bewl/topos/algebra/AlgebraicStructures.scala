package com.fdilke.bewl.topos.algebra

import com.fdilke.bewl.topos._

trait AlgebraicStructures extends
  BaseTopos with
  ToposEnrichments with
  ToposStructures {

  Ɛ: ToposPrerequisites =>

  import NamedLaws._
  import StandardTermsAndOperators._

  lazy val monoids = AlgebraicTheory(ι, *)(
    "left unit" law ( ι * α := α ),
    "right unit" law ( α * ι := α ),
    "associative" law ( (α * β) * γ := α * (β * γ ) )
  )

  trait CommutativityCriterion { algebra: Algebra =>
    def isCommutative = satisfies(
      α * β := β * α
    )
  }

  trait Actions[M <: ~] {
    val carrier: DOT[M]
    val unit: NullaryOp[M]
    val multiply: BinaryOp[M]
    
    trait ActionAnalysis[
      A <: ~,
      ANALYSIS[B <: ~] <: ActionAnalysis[B, ANALYSIS]
    ] {
      val action: Action[A]

      def morphismsTo[B <: ~](
        target: ANALYSIS[B]
      ): Iterable[A > B]

      def rawExponential[B <: ~](
        target: ANALYSIS[B]
      ): RawExponential[A, B]
    }
    
    trait ActionAnalyzer {
      type ANALYSIS[A <: ~] <: ActionAnalysis[A, ANALYSIS]
      def analyze[A <: ~](
        action: Action[A]
      ) : ANALYSIS[A]
    }

    trait PresentedAction[A <: ~] {
      val action: Action[A] 
      def project[B <: ~](
        otherAction: Action[B],
        targetElements: Seq[B]
      ): A > B
      def sanityTest: Unit
    }

    trait RawExponential[S <: ~, T <: ~] {
      val exponentialAction: Action[M x S → T]
      val evaluation: BiArrow[M x S → T, S, T]
      def transpose[X <: ~](
          otherAction: Action[X],
          biArrow: BiArrow[X, S, T]
      ): X > (M x S → T)
    }
    
    lazy val actions =
      AlgebraicTheoryWithScalars(
        carrier
      ) (
        II := unit
      ) (
        II, **, ***
      ) (
        "right unit" law ( α ** II := α ),
        "mixed associative" law ( (α ** Φ) ** Ψ := α ** (Φ *** Ψ) )
      )

    def action[A <: ~](
      actionCarrier: DOT[A]
    ) (
      actionMultiply: (A, M) => A
    ) =
      Action[A](
        actionCarrier, 
        (actionCarrier x carrier).biArrow(
          actionCarrier
        )(
          actionMultiply
        )
      )

    lazy val regularAction =
      action(carrier) {
        multiply(_, _)
      }

    def trivialAction[A <: ~](
      rawCarrier: DOT[A]
    ) =
      action(rawCarrier) {
        (a, m) => a
      }

    lazy val voidAction: Action[VOID] =
      trivialAction(O)

    case class Action[A <: ~](
      actionCarrier: DOT[A],
      actionMultiply: BiArrow[A, M, A]
    ) extends actions.Algebra[A](actionCarrier)(
      ** := actionMultiply,
      *** := multiply
    ) { 
      // Formalism to make the product of two Actions an Action to facilitate sugar
      def x[B <: ~](
        that: Action[B]
      ): Action[A x B] = {
        val product = (this : actions.Algebra[A])x(that)
        new Action[A x B](
          product.carrier,
          product.operatorAssignments.lookup(**).get
        )
      }
    }

    case class ActionPreArrow[
      S <: ~,
      T <: ~
    ] (
      source: Action[S],
      target: Action[T],
      function: S => T
    )
  }

  class Monoid[M <: ~](
    override val carrier: DOT[M],
    override val unit: NullaryOp[M],
    override val multiply: BinaryOp[M]
  ) extends monoids.Algebra[M](carrier)(
    ι := unit,
    * := multiply
  ) with Actions[M] with CommutativityCriterion
  
  lazy val groups = AlgebraicTheory(ι, $tilde, *)( // TODO: compiler accepts ~, IDE not
    "left unit" law ( ι * α := α ),
    "right unit" law ( α * ι := α ),
    "left inverse" law ( (~α) * α := ι ),
    "associative" law ( (α * β) * γ := α * (β * γ ) )
  )

  class Group[G <: ~](
    override val carrier: DOT[G],
    override val unit: NullaryOp[G],
    override val multiply: BinaryOp[G],
    val inverse: UnaryOp[G]
  ) extends groups.Algebra[G](carrier)(
    ι := unit,
    * := multiply,
    $tilde := inverse
  ) with Actions[G] with CommutativityCriterion {
    lazy val asMonoid = new Monoid[G](carrier, unit, multiply)
    
    // Formalism to make the product of two Groups a Group so we can add sugar here
    def x[H <: ~](
      that: Group[H]
    ): Group[G x H] = {
      val product = (this : groups.Algebra[G])x(that)
      new Group[G x H](
        product.carrier,
        product.operatorAssignments.lookup(ι).get,
        product.operatorAssignments.lookup(*).get,
        product.operatorAssignments.lookup($tilde).get
      )
    }
  }

  lazy val lattices = AlgebraicTheory(⊥, ⊤, ∨, ∧)(
    "commutative ∨" law ( α ∨ β := β ∨ α ),
    "associative ∨" law ( (α ∨ β) ∨ γ := α ∨ (β ∨ γ) ),
    "unit ⊥ for ∨" law ( ⊥ ∨ α := α  ),

    "commutative ∧" law ( α ∧ β := β ∧ α ),
    "associative ∧" law ( (α ∧ β) ∧ γ := α ∧ (β ∧ γ) ),
    "unit ⊤ for ∧" law ( ⊤ ∧ α := α  ),

    "absorptive ∧ over ∨" law ( α ∧ (α ∨ β) := α ),
    "absorptive ∨ over ∧" law ( α ∨ (α ∧ β) := α )
  )

  class Lattice[L <: ~](
    override val carrier: DOT[L],
    val bottom: NullaryOp[L],
    val top: NullaryOp[L],
    val meet: BinaryOp[L],
    val join: BinaryOp[L]
  ) extends lattices.Algebra[L](carrier)(
    ⊥ := bottom,
    ⊤ := top,
    ∧ := meet,
    ∨ := join
  )

  lazy val heytingAlgebras = lattices.extend(
    →
  )(
    "self implication" law ( α → α := ⊤ ),
    "modus ponens" law ( α ∧ (α → β) := α ∧ β ),
    "implication supersedes" law ( α ∧ (β → α) := α ),
    "left distributive →/∧" law ( α → (β ∧ γ) := (α → β) ∧ (α → γ) )
  )

  class HeytingAlgebra[H <: ~](
    override val carrier: DOT[H],
    val bottom: NullaryOp[H],
    val top: NullaryOp[H],
    val meet: BinaryOp[H],
    val join: BinaryOp[H],
    val implies: BinaryOp[H]
  ) extends heytingAlgebras.Algebra[H](
    carrier
  )(
    ⊥ := bottom,
    ⊤ := top,
    ∧ := meet,
    ∨ := join,
    → := implies
  )
}
