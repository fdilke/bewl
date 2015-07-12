# Project Bewl

A DSL for the internal (Mitchell-Benabou) language of a topos.

This [animated video](http://www.youtube.com/watch/?v=nUwjGBHXKYs) attempts a gentle introduction to Project Bewl (formerly Bile).
See also [this presentation](https://www.evernote.com/shard/s141/sh/8e6b9d94-bc20-4fde-b2bf-9e844f486f76/d11244bad0729071fa00d19eaad312ce)

I've implemented this in Java, Clojure and now Scala. Really it should be in Haskell but I am only partway through
[learning the language](http://learnyouahaskell.com)

[This presentation](http://prezi.com/dwrz2mft3y-g/?utm_campaign=share&utm_medium=copy&rc=ex0share) explains the new "intrinsic" Bewl 2.0 DSL and why it's
 better than the previous "diagrammatic" 1.0 one.

If you want to use Bewl as a learning aid to study category theory, [start here](https://github.com/fdilke/bewl/blob/master/CommandLine.md)

# Intended applications

- Explore the topos of graphs. Bewl will let us talk about graphs as if they were sets
- Explore fuzzy sets (using the more careful definition that makes them into a topos)
- Explore parity in other topoi (as it's so poorly understood for sets)
- Explore Lawvere-Tierney topologies (and perhaps save [these](http://www.math.uchicago.edu/~may/VIGRE/VIGRE2007/REUPapers/FINALFULL/Bartlett.pdf) 
[poor](http://user.cs.tu-berlin.de/~noll/ToposOfTriads.pdf) music theorists from having to calculate them by hand)

# Done so far

- Created Topos API as a trait
- Implemented FiniteSets as a topos
- Added 'generic topos tests' which use fixtures for the given topos
- Can define and verify algebraic laws. Only 'monotyped' laws as yet - can't define monoid and ring actions 
- compact notation for elements / lambdas / uniform operators
- adapter for diagrammatic layer
- Separated BaseTopos from its extra layers (Algebra, AlgebraicStructures) which are now traits
- Universal and existential quantifiers
- Strong binding: a new layer with stars/quivers concreting over dots/arrows ; stars bound to classes ; quivers/functions interchangeable
- Specialized element types: e.g. product has left, right ; exponential can apply
- Adapter that makes a dots-and-arrows topos look like a stars-and-quivers one
- Implementation of FiniteSets using new DSL
- Defined quantifiers in new DSL
- Implemented the topos of actions for a monoid
- Construct the initial object 0
- Coproducts
- predicates isMonic, isEpic, isIso
- enumerate morphisms / global elements
- Partial arrow classifier

# Tech tasks done
- cleaner memoization; optionally uses generics and lower bound types; works with structural types 

# To do

- Structure definitions for monoid, group, abelian group, ring with their respective algebraic laws
- Can extend and remap algebraic structures, e.g. ring extends abelian group remaps group with an extra law
- Broaden definition of algebraic structures to include models and parameterized operations
- Formalize constructions like: automorphism group, endomorphism ring, group of invertible elements of a monoid
- Extract the Heyting algebra structure of the subobject classifier
- colimits
- Optimize algorithm enough to construct triadic topos and its topologies

# Questions

- How would the powers work if they absolutely had to be typesafe? How would they work in Haskell? Would this
work if we could construct and dissect product binding types like (A, B, C) properly? Would 'dependent types' help?
