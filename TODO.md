# To do

- Tidy definitions of laws. 
- Define group, ring, etc
- Broaden definition of algebraic structures to include models and parameterized operations
- Formalize constructions like: automorphism group, endomorphism ring, group of invertible elements of a monoid
- Extract the Heyting algebra structure of the subobject classifier
- Construct coproducts and colimits (how does McLarty do this?)

# Questions

- Is there a way for the algebraic structure code to be taken outside the Topos trait?
- (potential answer:) Can we have an 'implicit topos'? Will that work when several are in play?
- Can have an Algebra class/object wrapped around a Topos#Dot that imports everything from the relevant topos?
- Can there be a Scala equivalent to the Clojure 'deflaw' to make it easier to specify algebraic laws?
- How to add 'parameterized operations' without doing even more violence to the type system? Shapeless?
- Can generalize IntegerPower and make it type safe?
