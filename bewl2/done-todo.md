
# TOO GNARLY (for now)
- default RESULT type of Unit for situations like withAction[Int, Unit] ?
  not quite, but could sort of do it with 'import experimental.namedTypeArguments'
- refactor OperatorAssignment to be table-driven
- in StockStructures, we laboriously implement left/right dominant monoids
- nice if there could be a CompleteHA and not just a HA - need infinitary operations/laws
- instead of summon[Dot[X]] use Dot[X], as with Mappable... bad idea: then have to use 'new'
- extension instead of RichArrow.. bad idea, we then get confused about 'o'

# DONE
- done_todo.md as motivator inside project, or should these notes live there?
- can protect Dot constructor?
- locally cache products, so Dot[(X, Y)] lives inside the cache for Dot[X]. Ditto exponentials
- there should really be tests for Memoize.type1 (didn't clone during refactor)
- asMonoid => withMonoid
- groups should be able to have actions
- we don't need to store a mappable
- biarrows acting on arrows: f(g, h) where g: X ~> Y, h: X ~> Z, f:(Y, Z) ~> W
- convert in-code TODOs to entries here
- sort out RichDot... There is none
- logical operations: construct the Heyting algebra of truth
- for logical operations' Boolean operators extension, use an implicit class instead and then don't need StandardXxx.<and>
- proper names/scopes for the ∀ methods
- VanillaAlgebraicTheory instead of AlgebraicTheory[Unit]? Or maybe Variety.

# TODO
- permutations, deep property of FinSet from card trick?
- argument for storing UNIT, BEWL, etc as types within the topos, not type parameters?
  Saves passing a load of baggage around. Implications for users?
- disentangle mask[]/sillyMask[] which should be a pure type operation
- any implicits left in tests for algebraic machinery? any other unnecessary flummery with '?=>' ?
- can improve how equalizerSituation / scope of implicits works in the generic topos tests?
- instead of group x group: bake this in, as with products of dots; have an implicit. Same for actions
- fix arrow.isIso which currently always returns true. also epic, mono. also ideally section/retraction, image
- 'rich arrow' features - separate tests for these
- TODO: override, test equivalents for logical operations
- role for those "(+) = ⊕" tensorial extractors instead of cumbersome ab_ω.map { _._1 } etc in LogicalOps and elsewhere
- expunge @targetName, no one is using Java to access this
- quantifiers -do same job for ∃ as for ∀. Plan on ∃[X], ∃[X, Y] separate operations. Lump in tests with logical ops.
