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

# Getting to Strong Binding

How to evolve the existing code to get there?

- First require FiniteSets[] to take a type parameter that is an element.
- Under the covers this will be completely disregarded, we are still manipulating ordinary objects.

conflict is: a topos object may in fact be parameterized by X, but we want to represent it by Y - in a type safe manner!
could say: a dot is fundamentally a Traversable[Any], and an Arrow a Function[Any, Any] on a specified domain,
so we throw away type safety at this level. But why should we have to?

Want to have: type safe dots for X, Y and functions X -> Y
wrapped in: type safe dots A, B, functions A -> B
Perhaps:
    FiniteSetsDot[X, Y] extends Dot[Y]
We are concreting over the type system, and expecting to do it type safely.
    
Trying to do too many things at once?
- formally have all types be an Element 
- experiment with new notation for products/exponents
- lift up the successful 'binding' mechanisms in Java

Start with 1, even if this means junking the existing rather weak type safety in FiniteSets.    
But: isn't it still useful to know what the concreted-over layer in the experimental StrongBinding
would look like?

We did have: a 'product diagram' = a dot with 2 projections, and a multiply operation.
Now: we have a 'product dot' whose type parameter encodes the projections, 
and a multiply operation on arrows.
If we can do this at all, we can do it as a layer over an existing topos.
For now, ignore the inefficiency of this. Eventually 'drive' the design down so that it's
built into the lower layer. And this would be a good point to re-TDD FiniteSets. So:

1. Build an inelegant layer on top of Topos, using the new elements etc. Get the API right.
2. Integrate this into a new FiniteSets, now that we know what the API ought to look like. 
In fact:
2a. Push DOT, ARROW down into FiniteSets, rewriting everything else in terms of Star/Quiver
2b. Get rid of DOT, ARROW

So: The product object is going to be a new Star with the 'real' product inside it.
Is every Star just this? Can retool WrappedStar.
Becoming clearer: the 'right' way to do FiniteSets is to treat the elements as Object, and
just get the types right, with the type-safety not helping at all with the mechanics.

Continuing with StrongBinding: If I can get the products right, this will show the way to
everything else.
Want to make a Quiver[S, T] interchangeable with a T because: T is just a wrapper for an arrow.
For this to work: when constructing composite types like Product and Exponent,
    the 'host' Star[T] has to know how to make a T out of an arrow S->T,
    because it's just a question of defining the right operations.
    Algebras ought to work like this too.
Aim for: it'll be *inherent* that if A is an X-algebra, A^B is automatically one too.
    
So... We have 'VanillaStarWrapper' for ordinary dots, and then ProductStarWrapper, AlgebraStarWrapper, etc
    for 'rich' dots with additional operations.
