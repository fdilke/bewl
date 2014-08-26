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

# Ideas for Strong Binding

I want to make an ARROW[X, Y] interchangeable with a Y. Here's how this might work.
- The 'marker types' X, Y, ... used as parameters in DOT and ARROW all stipulate X <: Element[X].
- ARROW[X, Y] has a method asElement which returns a Y.
- Element[Y] has a method asArrow(source: DOT[X]) which turns it into an ARROW[X, Y] again.
- The idea is that the marker types can include classes representing concepts like
 products, exponentials, the subobject classifier, etc which then have special methods.
- There will be a lambda() utility that lets you compose arrows in a functional notation.
- There can now be elegant definitions for things like the existential quantifier (currently a mess). 
- This is where the DSL really starts to acquire some power.
- Another testbed for this emerging DSL will be the algebraic constructions - endomorphism ring of a
module, etc.
- The way this will all work for sets: FiniteSetDot[X] will be a DOT[WrapperElement[X]], where WrapperElement
simply wraps an arrow.
- We will also generically wrap the DOT[POOR_OMEGA] into a DOT{RICH_OMEGA] which knows about all the
Heyting operations.
- Ideally, the tricksy special-case logic for nullary operators could be moved to asArrow(), which just 
turns an arrow 1 -> Y to an arrow X -> Y when necessary.
- Hopefully, this will sweep away all the rather unlovely plumbing for handling transposes, components,
characteristic functions, etc.
- It should also now be possible to deal with products in a (more) type-safe way, opening up the
possibility of 'parametric arities'. Probably every wrapped product arrow should include an array of type tags.


