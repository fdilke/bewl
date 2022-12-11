# NON-CODING
- sanity test symmetric group S_6 with '-Xmx3G'
- move algo stuff to another repo; it doesn't belong here. Ditto cocantorian stuff

# SPECULATIVE
- could Scala macros enable even deeper integration for Bewl? Or functional mottoes at least?
- added morphism enumeration. So committed to local finiteness. Could this be lifted?

# THEORETICAL
- L-T topologies. Can think of as quantifiers? Kernels of Heyting morphisms as per Matt F's paper?
- can model geometric morphisms, X: Topos -> Y: Topos?
- read Moerdijk & MacLane again, also the TTT book, and Four Czechs
- work out details of 'metrics' approach to H-valued sets:
  like top trumps / book or record details / mappings to DISCRETE or LINEAR?
- how much of the theory of (finite) groups carries over to LFTs?

# BEWL AS TOOL
- for card shuffle calc: verify surjectivity, investigate uniformity. Good stretch goal

# CONJECTURES ABOUT LFTs (locally finite topoi)
- ?every module has an injective hull
- ? Wedderburn holds - every division ring is a field - may follow mechanically from Brauer group machinery
- can we embed any X in an injective by considering X -> omega^X, extending to endo of omega^X ... ?
- is an image of an injective also injective? It is for sets, trivially. Fails in action category?
- Verify theorem that the inj hull of a subdirectly irreducible is again such. Finitely many of these?
- Is there always a generator? And therefore a cogenerator..?
- can we construct the injective hull of a module? is this guaranteed to exist? Say even in FinSet?
  Yes by this paper: https://www.ias.ac.in/article/fulltext/pmsc/109/04/0345-0351
  There is another paper somewhere which gives an algorithm for constructing it (algos on finite rings)

# TOO GNARLY (for now)
- default RESULT type of Unit for situations like withAction[Int, Unit] ?
  not quite, but could sort of do it with 'import experimental.namedTypeArguments'
- refactor OperatorAssignment to be table-driven
- in StockStructures, we laboriously implement left/right dominant monoids
- nice if there could be a CompleteHA and not just a HA - need infinitary operations/laws
- instead of summon[Dot[X]] use Dot[X], as with Mappable... bad idea: then have to use 'new'
- extension instead of RichArrow.. bad idea, we then get confused about 'o'
- unbundle stuff that doesn't need to be part of the topos proper: logic? algebraic theories?
- turn Mappable into ProductMappable, an annoying kid brother of one of the Haskell submonadics
  We can't - it's specific to DOT[_], at least potentially. Review when there are more topoi.
- argument for storing UNIT, BEWL, etc as types within the topos, not type parameters?
  Saves passing a load of baggage around. Implications for users? Performance? Test calc of S_5. Foresee imports clunkier.

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
- expunge @targetName, no one is using Java to access this
- disentangle mask[]/sillyMask[] which should be a pure type operation
- enumerate morphisms
- abstract code used for suits into withEnum ; remap the Direction code via this
- get tests working with Metals - do via the Flask icon for convenience. Or a launch does it
- withAutomorphismGroup which condenses withEndomorphismMonoid and withGroupOfUnits. Refactor existing uses
- fix arrow.isIso which currently always returns true. also epic, mono. determine image
- 'rich arrow' features - separate tests for these (need full quantifiers)
- does the symmetry group come with an action on n? it should do. Can use for card shuffle calc

# TODO
- permutations, deep property of FinSet from card trick?
- any implicits left in tests for algebraic machinery? any other unnecessary flummery with '?=>' ?
- can improve how equalizerSituation / scope of implicits works in the generic topos tests?
- instead of group x group: bake this in, as with products of dots; have an implicit. Same for actions
- TODO: override, test equivalents for logical operations
- can easily do section and retraction? what did we use those for in Bewl 1?
- topos of permutations
- topos of group actions. Should be a separate class
- when we have the topos of monoid actions, test topos.isBoolean
- could there be: generalized algebraic structures (encompassing varieties, ones with scalars, HAs) so we could loop
  over a sequence of 'forms' (e.g. something representing NullaryOp[Principal]) and see all structures on a dot?
- can I optimize away 'applicate'? I don't even understand what it does
- define structures for rings and modules ; calculate the endomorphism ring of a module. Do homology? Injective hulls?
- should productMagic be incorporated into Mappable so it's Map'n'Productable?
- image factorizations. do we need congruences for this?
- if we have rich internal arrows, X > Y (postponed above) test for being monic/epic/iso, compose 'em (caching the machinery...)
- can we have Option[X] implicitly generating a dot that is X*?
- compute the inverse of a morphism. Again inefficient before. Can we just backdivide from the identity?
- minor: can I have GenericToposSpec(using Sets) rather than bodgily passing an implicit with an extra set of parens
- use withAutos/withEndos pattern again for algebras; loop over the sequence of operators
- replace ~> with > (both of them), > with ~> ?
- 'idiomatic' use of operations, so we should be able to talk about a * g if a suitable group and action are in scope
- if calculating S_13 via endos-then-units is too much, use special purpose code from symm-group, integrate?
- formalize the 'drivers' by putting them all in a ToposDrivers trait? There are likely to be more.
- lose the implicit exponential [X > X], shouldn't need it
- fix up so: Group extends Actions[Group], then induction and preservation can be type safe
- coproducts. Definitely have a driver architecture for these. Use Either or | as we used tuples for products?
- full driver pattern/test for logops, autos, coproducts