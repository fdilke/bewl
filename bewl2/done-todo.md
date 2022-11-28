# THEORETICAL
- L-T topologies. Can think of as quantifiers? Kernels of Heyting morphisms as per Matt F's paper?
- can model geometric morphisms, X: Topos -> Y: Topos?
- read Moerdijk & MacLane again, also the TTT book, and Four Czechs

# TOO GNARLY (for now)
- default RESULT type of Unit for situations like withAction[Int, Unit] ?
  not quite, but could sort of do it with 'import experimental.namedTypeArguments'
- refactor OperatorAssignment to be table-driven
- in StockStructures, we laboriously implement left/right dominant monoids
- nice if there could be a CompleteHA and not just a HA - need infinitary operations/laws
- instead of summon[Dot[X]] use Dot[X], as with Mappable... bad idea: then have to use 'new'
- extension instead of RichArrow.. bad idea, we then get confused about 'o'
- unbundle stuff that doesn't need to be part of the topos proper: logic? algebraic theories?
 
# LEGITIMATELY POSTPONED
- simplify ∃ def by adding an extension so we can have f(x) instead of eval(f, x) for f: X ~> Y.
  Also simplify use of eval(fnBarBaz, cBar) in generic. Do this when there's more code, see if it affects performance

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
- role for those "(+) = ⊕" tensorial extractors instead of cumbersome ab_ω.map { _._1 } etc in LogicalOps and elsewhere
- quantifiers -do same job for ∃ as for ∀. Plan on ∃[X], ∃[X, Y] separate operations. Separate tests from logical ops.
- cache ∃[X], ∀[X] inside the dot
- wherever we have f(a ⊕ b) consider an extension of biarrows so it can be f(a, b)
- convenience untupling version of Equalizer.restrict() so that I don't need 'case x ⊕ y' in defining endo monoids
- finesse away direct references to CTXT in cases of untupling: just need type <>[X, Y, Z] = (CTXT[X], CTXT[Y]) => CTXT[Z] or sn

# TODO
- permutations, deep property of FinSet from card trick?
- argument for storing UNIT, BEWL, etc as types within the topos, not type parameters?
  Saves passing a load of baggage around. Implications for users?
- disentangle mask[]/sillyMask[] which should be a pure type operation
- any implicits left in tests for algebraic machinery? any other unnecessary flummery with '?=>' ?
- can improve how equalizerSituation / scope of implicits works in the generic topos tests?
- instead of group x group: bake this in, as with products of dots; have an implicit. Same for actions
- 'rich arrow' features - separate tests for these (need full quantifiers)
- fix arrow.isIso which currently always returns true. also epic, mono. also ideally section/retraction, image
- TODO: override, test equivalents for logical operations
- expunge @targetName, no one is using Java to access this
- topos of permutations
- topos of group actions. Should be a separate class
- when we have the topos of monoid actions, test topos.isBoolean
- could there be: generalized algebraic structures (encompassing varieties, ones with scalars, HAs) so we could loop
  over a sequence of 'forms' (e.g. something representing NullaryOp[Principal]) and see all structures on a dot?
- can I optimize away 'applicate'? I don't even understand what it does
- define structures for rings and modules ; calculate the endomorphism ring of a module. Do homology? Injective hulls?
- should productMagic be incorporated into Mappable so it's Map'n'Productable?
- 
