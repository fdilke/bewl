# Project Bewl

A DSL for the internal (Mitchell-Benabou) language of a topos.

This [animated video](http://www.youtube.com/watch/?v=nUwjGBHXKYs) attempts a gentle introduction to Project Bewl (formerly Bile)

I've implemented this in Java, Clojure and now Scala. Really it should be in Haskell but I am only partway through
[Barely Remember you a Hask](http://learnyouahaskell.com)


# Intended applications

- Explore the topos of graphs. Bewl will let us talk about graphs as if they were sets
- Explore parity in other topoi (as it's so poorly understood for sets)

# Done so far

- Created Topos API as a trait
- Implemented FiniteSets as a topos
- Added 'generic topos tests' which use fixtures for the given topos
- Can define and verify algebraic laws. Only 'monotyped' laws as yet - can't define monoid and ring actions 
- Separated BaseTopos from its extra layers (Algebra, AlgebraicStructures) which are now traits
- Structure definitions for monoid, group, abelian group, ring with their respective algebraic laws
- Can extend and remap algebraic structures, e.g. ring extends abelian group remaps group with an extra law

# To do

- Broaden definition of algebraic structures to include models and parameterized operations
- Formalize constructions like: automorphism group, endomorphism ring, group of invertible elements of a monoid
- Extract the Heyting algebra structure of the subobject classifier
- Construct coproducts and colimits
- Add bindings: the type arguments have their own methods e.g. product has left, right

# Questions

- How to add 'parameterized operations' without doing even more violence to the type system
 than I already did with IntegerPower?
- How would the powers work if they absolutely had to be typesafe? How would they work in Haskell? Would this
work if we could construct and dissect product binding types like (A, B, C) properly? Would 'dependent types' help?
