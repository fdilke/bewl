package com.fdilke.bewl2.algebra

import com.fdilke.bewl2.Mappable
import com.fdilke.bewl2.Topos
import com.fdilke.utility.Shortcuts.*

import scala.annotation.targetName
import scala.language.{dynamics, postfixOps}
import Mappable._

trait AlgebraicStructures[
  DOT[_],
  CTXT[_]: Mappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
] extends AlgebraicTheories[DOT, CTXT, VOID, UNIT, BEWL, >] {
  topos: Topos[DOT, CTXT, VOID, UNIT, BEWL, >] =>

  import StandardTermsAndOperators._
  import StandardTermsAndOperators.!
  import StandardTermsAndOperators.*

  val groups: AlgebraicTheory[UNIT] =
    AlgebraicTheory(ι, !, *)(
      "left unit" law {
        ι * α := α
      },
      "right unit" law {
        α * ι := α
      },
      "left inverse" law {
        (!α) * α := ι
      },
      "associative" law {
        (α * β) * γ := α * (β * γ)
      }
    )

  class Group[G: DOT](
    val unit: NullaryOp[G],
    val multiply: BinaryOp[G],
    val inverse: UnaryOp[G]
  ) extends groups.Algebra[G](
    ι := unit,
    * := multiply,
    (!) := inverse
  ) :
    def x[H: DOT]( // product sugar
      that: Group[H]
    ): Group[(G, H)] = {
      val product = (this: groups.Algebra[G]) x that
      new Group[(G, H)](
        product.operatorAssignments.lookup(ι).get,
        product.operatorAssignments.lookup(*).get,
        product.operatorAssignments.lookup(!).get
      )
    }
    lazy val asMonoid: Monoid[G] =
      new Monoid[G](unit, multiply)

  val monoids: AlgebraicTheory[UNIT] =
    AlgebraicTheory(ι, *)(
      "left unit" law (ι * α := α),
      "right unit" law (α * ι := α),
      "associative" law ((α * β) * γ := α * (β * γ))
    )

  class Monoid[M: DOT](
    val unit: NullaryOp[M],
    val multiply: BinaryOp[M]
  ) extends monoids.Algebra[M](
    ι := unit,
    * := multiply
  ) with Actions[M]

  trait Actions[M: DOT] {
    val unit: NullaryOp[M]
    val multiply: BinaryOp[M]

    val actions: AlgebraicTheory[M] =
      AlgebraicTheoryWithScalars[M](
        II := unit,
        *** := multiply
      )(
        II, **, ***
      )( laws =
        "right unit" law (α ** II := α),
        "mixed associative" law ((α ** Φ) ** Ψ := α ** (Φ *** Ψ))
      )

    def action[A: DOT](
      actionMultiply: (A, M) ~> A
    ): Action[A] =
      Action[A](actionMultiply)

    lazy val regularAction: Action[M] =
      action[M](multiply)

    def trivialAction[A: DOT]: Action[A] =
      action[A]{ a_m => a_m.map{ _._1 } }

    lazy val voidAction: Action[VOID] =
      trivialAction[VOID]

    class Action[A: DOT](
     actionMultiply: BiArrow[A, M, A]
   ) extends actions.Algebra[A](
      ** := actionMultiply
    ) {
      // Formalism to make the product of two Actions an Action to facilitate sugar
      def x[B: DOT](
        that: Action[B]
      ): Action[(A, B)] = {
        val product = (this: actions.Algebra[A]) x that
        new Action[(A, B)](
          product.operatorAssignments.lookup(**).get
        )
      }
    }
  }

  extension(a: Algebra)
    def isCommutative = a.satisfies(
      α * β := β * α
    )

  val lattices: AlgebraicTheory[UNIT] =
    AlgebraicTheory(⊥, ⊤, ∨, ∧)(
      "commutative ∨" law (α ∨ β := β ∨ α),
      "associative ∨" law ((α ∨ β) ∨ γ := α ∨ (β ∨ γ)),
      "unit ⊥ for ∨" law (⊥ ∨ α := α),

      "commutative ∧" law (α ∧ β := β ∧ α),
      "associative ∧" law ((α ∧ β) ∧ γ := α ∧ (β ∧ γ)),
      "unit ⊤ for ∧" law (⊤ ∧ α := α),

      "absorptive ∧ over ∨" law (α ∧ (α ∨ β) := α),
      "absorptive ∨ over ∧" law (α ∨ (α ∧ β) := α)
    )

  class Lattice[L: DOT](
    val bottom: NullaryOp[L],
    val top: NullaryOp[L],
    val meet: BinaryOp[L],
    val join: BinaryOp[L]
  ) extends lattices.Algebra[L](
    ⊥ := bottom,
    ⊤ := top,
    ∧ := meet,
    ∨ := join
  )

  val heytingAlgebras: AlgebraicTheory[UNIT] =
    lattices.extend(→)(moreLaws =
      "self implication" law (α → α := ⊤),
      "modus ponens" law (α ∧ (α → β) := α ∧ β),
      "implication supersedes" law (α ∧ (β → α) := α),
      "left distributive →/∧" law (α → (β ∧ γ) := (α → β) ∧ (α → γ))
    )

  class HeytingAlgebra[H: DOT](
    val bottom: NullaryOp[H],
    val top: NullaryOp[H],
    val meet: BinaryOp[H],
    val join: BinaryOp[H],
    val implies: BinaryOp[H]
  ) extends heytingAlgebras.Algebra[H](
    ⊥ := bottom,
    ⊤ := top,
    ∧ := meet,
    ∨ := join,
    → := implies
  )
}
