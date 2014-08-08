# To do

- Define group, ring, etc
- can 'isCommutative' be a method on Group, Ring? Requires some refactoring of law verification?
- Broaden definition of algebraic structures to include models and parameterized operations
- Formalize constructions like: automorphism group, endomorphism ring, group of invertible elements of a monoid
- can have one type of structure extending another, as we did in Clojure? e.g. ring extends abelian group?
- check arities of operators in a law?
- add tests based on these: we can verify that Aut(2) is commutative but Aut(3) isn't
- Extract the Heyting algebra structure of the subobject classifier
- Construct coproducts and colimits (how does McLarty do this?)

# Questions

- Is there a way for the algebraic structure code to be taken outside the Topos trait?
- (potential answer:) Can we have an 'implicit topos'? Will that work when several are in play?
- Can have an Algebra class/object wrapped around a Topos#Dot that imports everything from the relevant topos?
- Can there be a Scala equivalent to the Clojure 'deflaw' to make it easier to specify algebraic laws?
- How to add 'parameterized operations' without doing even more violence to the type system? Shapeless?
- Can generalize IntegerPower and make it type safe?
- Get to the bottom of why 'nullary operator remapping' is necessary. Why should 0 be a special case? Is it because we
can't be bothered to store the source of the operator arrow map, when it is only needed in the 0-arity case?
Probably AlgebraicOperator.apply() needs to be smarter... it needs to multiply over one source, but map to another.

That way we can have:
  AlgebraicOperator: R -> 1 -> X
be a 0-ary operator, and applying it to 0 variables causes a multiplication over 1
problem is: that we are trying to treat unary operators, X^n -> X with n = 0
the same as variables R = X^m -> X, and they're not. So:
=> the product over which we multiply for a unary operator is not always its source.
The operator is just R -> S = X^n -> X
Want to be able to do algebra with a bunch of 'variables' S =X^n -> X
 This works because:
  if x, y, ... all map R -> X
  then any op(x, y, ...) also maps R -> X because:
  multiply up to get R -> X^a, a = arity
  then apply the operator arrow. Aren't we already doing this in AlgebraicOperator?
