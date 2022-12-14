package com.fdilke.bewl2.algebra

import com.fdilke.bewl2.Mappable
import com.fdilke.bewl2.Topos
import com.fdilke.utility.Shortcuts.*

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

  val groups: Variety =
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

  class Group[G: Dot](
    val unit: NullaryOp[G],
    val multiply: BinaryOp[G],
    val inverse: UnaryOp[G]
  ) extends groups.Algebra[G](
    ι := unit,
    * := multiply,
    (!) := inverse
  ) with Actions[G] :
    def x[H: Dot]( // product sugar
      that: Group[H]
    ): Group[(G, H)] = {
      val product = (this: groups.Algebra[G]) x that
      new Group[(G, H)](
        product.operatorAssignments.lookup(ι).get,
        product.operatorAssignments.lookup(*).get,
        product.operatorAssignments.lookup(!).get
      )
    }
    private lazy val asMonoid: Monoid[G] =
      new Monoid[G](unit, multiply)
    def withMonoid[RESULT](
      block: Monoid[G] ?=> RESULT
    ): RESULT =
      given Monoid[G] = asMonoid
      block

  implicit class RichMonoidElement[M: Dot: Monoid](a: CTXT[M]):
    def *(b: CTXT[M]): CTXT[M] =
      summon[Monoid[M]].multiply(a, b)

  implicit class RichGroupElement[G: Dot: Group](a: CTXT[G]):
    def *(b: CTXT[G]): CTXT[G] =
      summon[Group[G]].multiply(a, b)
    def unary_~ =
      summon[Group[G]].inverse(a)

  val monoids: Variety =
    AlgebraicTheory(ι, *)(
      "left unit" law (ι * α := α),
      "right unit" law (α * ι := α),
      "associative" law ((α * β) * γ := α * (β * γ))
    )

  class Monoid[M: Dot](
    val unit: NullaryOp[M],
    val multiply: BinaryOp[M]
  ) extends monoids.Algebra[M](
    ι := unit,
    * := multiply
  ) with Actions[M]

  trait Actions[M: Dot]:
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

    def withAction[A: Dot, RESULT](
      actionMultiply: (A, M) ~> A
    )(
      block: Action[A] ?=> RESULT
    ): RESULT =
      given Action[A] = Action[A](actionMultiply)
      block

    def withRegularAction[RESULT](
      block: Action[M] ?=> RESULT
    ): RESULT =
      given Action[M] = Action[M](multiply)
      block

    def withTrivialAction[A: Dot, RESULT](
       block: Action[A] ?=> RESULT
    ): RESULT =
      given Action[A] = Action[A]{ case a ⊕ _ => a }
      block

    def withVoidAction[RESULT](
      block: Action[VOID] ?=> RESULT
    ): RESULT =
      withTrivialAction[VOID, RESULT] {
        block
      }

    class Action[A: Dot](
      val actionMultiply: BiArrow[A, M, A]
    ) extends actions.Algebra[A](
      ** := actionMultiply
    ):
      def x[B: Dot]( // Formalism to make the product of two Actions an Action to facilitate sugar
        that: Action[B]
      ): Action[(A, B)] =
        val product = (this: actions.Algebra[A]) x that
        new Action[(A, B)](
          product.operatorAssignments.lookup(**).get
        )

      def induced[G: Dot](
        morphism: G ~> M
      )(
        implicit group: Group[G]
      ): group.Action[A] =
        group.Action[A]{ case a ⊕ g =>
          actionMultiply(a, morphism(g))
        }

      def preserving[Q: Dot, RESULT](
        arrow: A ~> Q
      ) (
        block: [P] => (dotP : Dot[P]) ?=> (groupP: Group[P]) ?=> (embed: P ~> M) => RESULT
      ) (
        implicit groupM: Group[M] // note, this is a kludge
      ): RESULT =
        ∀[M, A] { (m, a) =>
          arrow(a) =?= arrow(actionMultiply(a, m))
        } whereTrue { [P] => (dotP: Dot[P]) ?=> (equalizer: Equalizer[P, M]) =>
          implicit val groupP: Group[P] = new Group[P](
            unit = equalizer.restrict(groupM.unit),
            multiply = equalizer.restrict { (p, q) => 
              groupM.multiply(
                equalizer.inclusion(p),
                equalizer.inclusion(q)
              )
            },
            inverse = equalizer.restrict { p =>
              groupM.inverse(equalizer.inclusion(p))
            }
          )
          block[P](equalizer.inclusion)
        }

      def toExponent: M ~> (A > A) =
        transpose[M, A, A] { case m ⊕ a => 
          actionMultiply(a, m)
        }

  extension(a: AlgebraicTheory[_]#Algebra[_])
    def isCommutative = a.satisfies(
      α * β := β * α
    )

  val lattices: Variety =
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

  class Lattice[L: Dot](
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

  val heytingAlgebras: Variety =
    lattices.extend(→)(moreLaws =
      "self implication" law (α → α := ⊤),
      "modus ponens" law (α ∧ (α → β) := α ∧ β),
      "implication supersedes" law (α ∧ (β → α) := α),
      "left distributive →/∧" law (α → (β ∧ γ) := (α → β) ∧ (α → γ))
    )

  class HeytingAlgebra[H: Dot](
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
