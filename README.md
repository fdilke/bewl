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
- Can define algebraic laws and structures. Only 'monotyped' laws as yet - can't define monoid and ring actions 


# To do

- Allow definition of algebraic structures with laws. Define group, ring, etc
- Broaden definition of algebraic structures to include models and parameterized operations
- Formalize constructions like: automorphism group, endomorphism ring, group of invertible elements of a monoid
- Extract the Heyting algebra structure of the subobject classifier
- Construct coproducts and colimits

# Questions

- Is there a way for the algebraic structure code to be taken outside the Topos trait?
- (potential answer:) Can we have an 'implicit topos'? Will that work when several are in play?
- Can avoid the messiness of passing sources around? Just how bad an idea would it be to use implicits?
- Can there be a Scala equivalent to the Clojure 'deflaw' to make it easier to specify algebraic laws?
- How to add 'parameterized operations' without doing even more violence to the type system
 than I already did with IntegerPower?
