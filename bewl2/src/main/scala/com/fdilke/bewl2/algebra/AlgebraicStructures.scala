package com.fdilke.bewl2.algebra

import com.fdilke.bewl2.topos.ProductMappable
import com.fdilke.bewl2.topos.Topos
import com.fdilke.utility.Shortcuts._

import scala.language.{dynamics, postfixOps}
import ProductMappable._

trait AlgebraicStructures[
  DOT[_],
  CTXT[_]: ProductMappable,
  VOID,
  UNIT,
  BEWL,
  >[_, _]
] extends AlgebraicTheories[DOT, CTXT, VOID, UNIT, BEWL, >]:
  topos: Topos[DOT, CTXT, VOID, UNIT, BEWL, >] =>

  import StandardTermsAndOperators._
  import StandardTermsAndOperators.!
  import StandardTermsAndOperators.*

  val groups: Variety =
    AlgebraicTheory(ι, !, *)(
      "left unit" law:
        ι * α := α
      ,
      "right unit" law:
        α * ι := α
      ,
      "left inverse" law:
        (!α) * α := ι
      ,
      "associative" law:
        (α * β) * γ := α * (β * γ)
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
    val dot: Dot[G] = summon

    lazy val actionTopos: Topos[Action, CTXT, VOID, UNIT, BEWL, >] =
      toposOfGroupActions(this)

    infix def x[H: Dot]( // product sugar
      that: Group[H]
    ): Group[(G, H)] =
      val product: groups.Algebra[(G, H)] =
        (this: groups.Algebra[G]) x that
      new Group[(G, H)](
        unit = product.operatorAssignments.lookup(ι).get,
        multiply = product.operatorAssignments.lookup(*).get,
        inverse = product.operatorAssignments.lookup(!).get
      )

    private lazy val asMonoid: Monoid[G] =
      new Monoid[G](unit, multiply)

    def withMonoid[RESULT](
      block: Monoid[G] ?=> RESULT
    ): RESULT =
      given Monoid[G] = asMonoid
      block

  implicit class RichMonoidElement[M: Dot: Monoid](a: CTXT[M]):
    inline def *(b: CTXT[M]): CTXT[M] =
      summon[Monoid[M]].multiply(a, b)

  implicit class RichGroupElement[
    G: Dot: Group
  ](
    a: CTXT[G]
  ):
    inline def *(b: CTXT[G]): CTXT[G] =
      summon[Group[G]].multiply(a, b)
    inline def unary_~ =
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
  ) with Actions[M]:
    val dot: Dot[M] = summon
    opaque type RightIdeal = M > BEWL

    lazy val actionTopos: Topos[Action, CTXT, VOID, UNIT, RightIdeal, InternalMap] =
      toposOfMonoidActions(this)

  trait Actions[M: Dot]:
    val unit: NullaryOp[M]
    val multiply: BinaryOp[M]
    opaque type InternalMap[T, U] = (M, T) > U
    
    // TODO: get rid of these by refactoring with Tagged[]
    def hackTag[TYPE[_], T, U](tag: TYPE[(M, T) > U]): TYPE[InternalMap[T, U]] = tag
    def reverseHackTag[TYPE[_], T, U](tag: TYPE[InternalMap[T, U]]): TYPE[(M, T) > U] = tag

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
      given Action[A] =
        Action[A](actionMultiply)
      block

    def withRegularAction[RESULT](
      block: Action[M] ?=> RESULT
    ): RESULT =
      given Action[M] = Action[M](multiply)
      block

    def withTrivialAction[A: Dot, RESULT](
       block: Action[A] ?=> RESULT
    ): RESULT =
      given Action[A] = Action[A]:
        case a ⊕ _ => a
      block

    def withVoidAction[RESULT](
      block: Action[VOID] ?=> RESULT
    ): RESULT =
      withTrivialAction[VOID, RESULT]:
        block

    class Action[A: Dot](
      val actionMultiply: BiArrow[A, M, A]
    ) extends actions.Algebra[A](
      ** := actionMultiply
    ):
      def dot: Dot[A] =
        summon
        
      infix def x[B: Dot]( // Formalism to make the product of two Actions an Action to facilitate sugar
        that: Action[B]
      ): Action[(A, B)] =
        val product = (this: actions.Algebra[A]) x that
        Action[(A, B)]:
          product.operatorAssignments.lookup(**).get

      def induced[G: Dot](
        morphism: G ~> M
      )(
        implicit group: Group[G]
      ): group.Action[A] =
        group.Action[A]:
          case a ⊕ g =>
            actionMultiply(a, morphism(g))

      def preserving[Q: Dot, RESULT](
        arrow: A ~> Q
      ) (
        block: [P] => (dotP : Dot[P]) ?=> (groupP: Group[P]) ?=> (embed: P ~> M) => RESULT
      ) (
        implicit groupM: Group[M] // note, this is a kludge. Better way to express that it's a group?
      ): RESULT =
        ∀[M, A]:(m, a) =>
          arrow(a) =?= arrow(actionMultiply(a, m))
        .whereTrue:
          [P] => (dotP: Dot[P]) ?=> (equalizer: Equalizer[P, M]) =>
          implicit val groupP: Group[P] = new Group[P](
            unit =
              equalizer.restrict(groupM.unit),
            multiply =
              equalizer.restrict: (p, q) =>
                groupM.multiply(
                  equalizer.inclusion(p),
                  equalizer.inclusion(q)
                )
            ,
            inverse = equalizer.restrict: p =>
              groupM.inverse:
                equalizer.inclusion(p)
          )
          block[P]:
            equalizer.inclusion

      def toExponent: M ~> (A > A) =
        transpose[M, A, A]:
          case m ⊕ a =>
            actionMultiply(a, m)

  extension(a: AlgebraicTheory[?]#Algebra[?])
    def isCommutative = a.satisfies:
      α * β := β * α

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
