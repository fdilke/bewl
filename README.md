# Project Bewl

A DSL for the internal (Mitchell-Benabou) language of a topos. See [this presentation](https://www.evernote.com/shard/s141/sh/8e6b9d94-bc20-4fde-b2bf-9e844f486f76/d11244bad0729071fa00d19eaad312ce)

I've had to keep re-implementing Bewl in successively more powerful programming languages (Java, Clojure and now Scala). 
Really it should be in Haskell or Idris but I'm not ready for those yet.

[This presentation](http://prezi.com/dwrz2mft3y-g/?utm_campaign=share&utm_medium=copy&rc=ex0share) explains the new "intrinsic" Bewl 2.0 DSL and why it's
 better than the previous "diagrammatic" 1.0 one.

If you want to use Bewl as a learning aid to study category theory, [start here](https://github.com/fdilke/bewl/blob/master/CommandLine.md).

Here's a presentation about Bewl's [universal algebra capabilities](https://github.com/fdilke/bewl/blob/master/notes/BewlUniversalAlgebra.pdf).

And here's one about the [topos of automorphisms](https://github.com/fdilke/bewl/blob/master/notes/ToposOfAutomorphisms.pdf), 
another chapter in the ongoing quest for a decent explanation of permutation parity.

This [animated video](http://www.youtube.com/watch/?v=nUwjGBHXKYs) was an initial attempt to explain Bewl (back when it was called Bile)

To see the current to-do list and state of play, you can view this [Trello board](https://trello.com/b/PfdnsRNl/bewl). There's also a 
[continuous integration setup](https://snap-ci.com/fdilke/bewl/branch/master) which runs all the tests on each commit.

# Intended applications

- Explore the topos of graphs. Bewl will let us talk about graphs as if they were sets
- Explore fuzzy sets (using the more careful definition that makes them into a topos)
- Explore parity in other topoi (as it's so poorly understood for sets). Easier now that the
"topos of permutations" is implemented.
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
- walkthrough for using Bewl as a learning aid for studying category theory
- universal algebra: can define algebraic structures, using existing ones as parameter spaces (for monoid actions)
- implemented topos of automorphisms

# To do

- Define more algebraic structures
- Can extend and remap algebraic structures, e.g. ring extends abelian group remaps group with an extra law
- More algebraic constructions: endomorphism ring, transfer
- Extract the Heyting algebra structure of the subobject classifier
- coequalizers
- tell if an object has an injective hull
- Optimize algorithm enough to construct triadic topos and its topologies
- Construct the slice topos. Use McLarty's construction, NOT the one in Moerdijk/Maclane which
requires you to first construct the power object for exponentials
