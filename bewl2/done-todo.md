
# TOO GNARLY (for now)
- default RESULT type of Unit for situations like withAction[Int, Unit] ?
  not quite, but could sort of do it with 'import experimental.namedTypeArguments'
- refactor OperatorAssignment to be table-driven
- in StockStructures, we laboriously implement left/right dominant monoids

# DONE
- done_todo.md as motivator inside project, or should these notes live there?
- can protect Dot constructor?
- locally cache products, so Dot[(X, Y)] lives inside the cache for Dot[X]. Ditto exponentials
- there should really be tests for Memoize.type1 (didn't clone during refactor)
- asMonoid => withMonoid
- groups should be able to have actions

# TODO
- permutations, deep property of FinSet from card trick?
- argument for storing UNIT, BEWL, etc as types within the topos, not type parameters?
  Saves passing a load of baggage around. Implications for users?
- disentangle mask[]/sillyMask[] which should be a pure type operation
- any implicits left in tests for algebraic machinery? any other unnecessary flummery with '?=>' ?
- can improve how equalizerSituation / scope of implicits works in the generic topos tests?
- convert in-code TODOs to entries here
- logical operations: construct the Heyting algebra of truth
- instead of group x group: bake this in, as with products of dots; have an implicit. Same for actions
- fix arrow.isIso which currently always returns true. also epic, mono. also ideally section/retraction, image
- nice if there could be a CompleteHA and not just a HA - need infinitary operations/laws
- TODO: override, test equivalents
- extension instead of RichArrow
- sort out RichDot
- proper names/scopes for the ∀ methods
- biarrows acting on arrows: f(g, h) where g: X ~> Y, h: X ~> Z, f:(Y, Z) ~> W
- instead of summon[Dot[X]] use Dot[X], as with Mappable
- we don't need to store a mappable
- role for those "(x)" tensorial extractors instead of cumbersome ab_ω.map { _._1 } etc in LogicalOps and elsewhere
-   xx 
- 