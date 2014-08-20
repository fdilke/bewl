# Lower level TODOs

- can 'isCommutative' be a method on Group, Ring? Requires some refactoring of law verification?
- Is the remapping of operators a hack that should be done some other way?
- check arities of operators in a law? Can't currently because they are just arrows. Is that fixable?
- add tests based on these: we can verify that Aut(2) is commutative but Aut(3) isn't
- Construct coproducts and colimits (how does McLarty do this?)

# Questions

- Can there be a Scala equivalent to the Clojure 'deflaw' to make it easier to specify algebraic laws?
- Is there a way for the algebraic structure code to be taken outside the Topos trait?
- (potential answer:) Can we have an 'implicit topos'? Will that work when several are in play?
- Can there be a Scala equivalent to the Clojure 'deflaw' to make it easier to specify algebraic laws?
- How to add 'parameterized operations' without doing even more violence to the type system? Shapeless?
- Can generalize IntegerPower and make it type safe?
