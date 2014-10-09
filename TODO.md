# Lower level TODOs

- can 'isCommutative' be a method on Group, Ring? Requires some refactoring of law verification?
- Is the remapping of operators a hack that should be done some other way?
- check arities of operators in a law? Can't currently because they are just arrows. Is that fixable?
- add tests based on these: we can verify that Aut(2) is commutative but Aut(3) isn't
- Construct coproducts and colimits (how does McLarty do this?)

# Questions

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

- Concrete over the type system, type safely. Stars and quivers replace dots and arrows. Do products.
- Rewrite exponentials, subobject classifier, etc in stars and quivers. 
- Build stars/quivers into the API for a topos. 
Instead pf 'product diagram' = a dot with 2 projections, and a multiply operation:
we have a 'product dot' whose type parameter encodes the projections, and a "x" operation on arrows.
At this point DOT, ARROW dissolve as implementation details somewhere in FiniteSets.

Algebras ought to extend the API of a Star.
Aim for: it'll be *inherent* that if A is an X-algebra, A^B is automatically one too.

# This is totally how I should implement algebraic operations:

use scala.Dynamic
http://stackoverflow.com/questions/15799811/how-does-type-dynamic-work-and-how-to-use-it/15799812#15799812        

# can tidy up syntax for using ResultStore? 

e.g. standardProductStar - monstrous code.        
        
# The Strong Binding Manifesto

Elements wrap arrows
Quivers wrap functions.
Stars wrap Dots and can:
    map an arrow into an element
Quivers wrap functions A => B (A,B elements) and also:    
    remember their source and target
    can test equality        

Abandoned the idea of regarding a quiver as an element. (Translation is possible.)

Abandoned the map notation for(x <- atar) because an element would have to know its target...
Could be rescued. In map() on a star, we make the identity arrow into an element,
    then invoke the function on it, then take the target of that... Seems unnecessary calc.
The functional notation is kind of better anyway.
May resuscitate this idea for operators.
    
Implementation:
Quivers have a lazily calculated arrow inside them.    
Their equality semantics work by calculating this.    

Eventually:
There will be an 'abstract star' API as for dots, and we'll rewrite sets in terms of this.

Possibly:
There'll be an 'old style topos API' and an adapter for making its dots and arrows
look like stars and quivers. Then we can keep all that machinery (product diagrams, etc)
and still write a 'new' FiniteSets in terms of dots and arrows:
OldFiniteSets extends OldTopos
ToposWrapper(OldTopos) extends NewTopos
NewFiniteSets extends NewTopos
...and consider whether to keep the old machinery when this is done.
Might be useful for upcoming topoi like 'M-actions', "H-sets'. 

