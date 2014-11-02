# TODO
sort out this todo list, it's a mess
rename ELEMENT to ~?
lose leftProjection, rightProjection by having indices on a biproduct
Can use symbols like ∀, ∃ for method names? Yes. So festoon code with symbols. How do you type them?
try 'lazy me' experiment with pairs

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

# Possibly useful for algebraic operations:

use scala.Dynamic
http://stackoverflow.com/questions/15799811/how-does-type-dynamic-work-and-how-to-use-it/15799812#15799812
Then have MonoidStar, HeytingAlgebraStar, etc        

# The Strong Binding Manifesto

Elements wrap arrows
Quivers wrap functions.
Stars wrap Dots and can:
    map an arrow into an element
Quivers wrap functions A => B (A,B elements) and also:    
    remember their source and target
    can test equality        

Elements are conceptually new - they represent what the stars can parameterize over. 

Instead of DIAGRAMS for product/equalizer/exponential/subobject classifier, we have subtypes of star
and element types already baked in to the language (functions, tuples) so it's all inherent. 

Implementation:
(for the adapter) Quivers have a lazily calculated arrow inside them.    
Their equality semantics work by calculating this.    

Currently:
Kept 'old style topos API' and there is an adapter for making its dots and arrows look like stars and quivers. 

# Algebraic Machinery

Algebras ought to extend the API of a Star.
Aim for: it'll be *inherent* that if A is an X-algebra, A^B is automatically one too.

Having spiked, work out a way to test-drive this from the bottom up. Some concepts we need:

- star tags: principal, rightScalar, etc 
- algebraic theories (include a map from star tags to stars)
- root context (a product, with a way to handle projections as variables)
- expressions that can be built up recursively, then evaluated in a root context to test algebraic laws
- abstract operators that operate on StarTag's
 - bound versions of them for a particular algebra, which can in fact be elements, and exist in the context
 of some quiver construction: root(component) { rootElement => ... }
 So we can be passing elements around by the time we are dealing with Variables.

Then maybe a Variable wraps an ELEMENT of the given type. How to make it all type safe??
Or better to have a sanity test on the operators as we build expressions? [MUCH better done at compile time] 
 
Can we easily associate a type with each StarTag? Maybe they should actually be types, or have types?
Could now do type-safe products? Would these be useful inside RootContext, or as a dress rehearsal for it?

Better to have Topos stripped down to use 'RichStar' and an implicit conversion, rather than include 
all the things the definition of a star doesn't have to have?
      
# Factoring through

Can we have a predicate "f factors through g'? Or even 'f factors through g uniquely'? Useful in tests.
f: R -> X factors through g: S -> X means:
(1) for each r, there exists s with f(r) = g(s)
(2) there exists h: R -> S with f = gh
