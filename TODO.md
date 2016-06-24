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

# Questions

- Can there be a Scala equivalent to the Clojure 'deflaw' to make it easier to specify algebraic laws?
YES: upcoming trait for theories/models/algebras

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
Note they don't have to be arrows from a specified 'root' object, only something whose nominal
manipulations can be used to construct quivers.

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
Or better to have a sanity test on the operators as we build expressions? (MUCH better done at compile time) 
 
Can we easily associate a type with each StarTag? Maybe they should actually be types, or have types?
Could now do type-safe products? Would these be useful inside RootContext, or as a dress rehearsal for it?

Better to have Topos stripped down to use 'RichStar' and an implicit conversion, rather than include 
all the things the definition of a star doesn't have to have?
      
# Factoring through

Can we have a predicate "f factors through g'? Or even 'f factors through g uniquely'? Useful in tests.
f: R -> X factors through g: S -> X means:
(1) for each r, there exists s with f(r) = g(s)
(2) there exists h: R -> S with f = gh

# idea: Linkages as metatypes for monoid actions
Encapsulate the fundamental unit:
  -  A and a RightAction\[A\]
  -  AA and an equivalence A \<-\> AA
Potentially enables a typesafe version of the (existing horrible) quiver code

# Possible DSL extensions
  Better to throw an exception if the function is ever called?
  Should we have ELEMENT\[X\] instead of ELEMENT? in the mapping functions, for example?

# Rewrapping: a rejected idea

  Should the 'rewrapping' methods in ElementProxy be part of the topos API?
  Corollary: Should an ELEMENT type always be an interface? (cosmic typing)
  Finessed this: For the action exponentials to work, we need there to be an underlying object
  which can be wrapped and rewrapped in multiple interfaces.
  Not an issue: Does it matter that when an element of M x A -> B is regarded as an A -> B,
  its function is no longer M-preserving? This seems acceptable only if the
  function is only there for show, and the user doesn't have access to it.
  Thankfully not needed: Can we arrange for a FiniteSet\[X\] to still ultimately be a Traversable\[X\], even if its
  interface makes it look like a STAR\[NEW_LAYER\[X\]\] where NEW_LAYER has the rewrapper methods?

# Integer powers: a rejected construction
involved too much violence to the type system
Can't see how to do these type safely. Maybe if I used typelevel Peano arithmetic for the integers?
Ultimately not needed - we can work quite happily with biproducts and adhoc variadic operators

# Loosening biproducts and exponentials: (for typesafe actions) : Still WIP

Currently: a > b has to be an EXPONENTIAL{A, B] = STAR\[A > B\] with ExponentialStar\[A, B\]
where A > B ::== (A => B) with ELEMENT

If > was user-defined, we could have:
A >M B ::== (M x A) > B with ELEMENT with (noddy evaluation)
We still have to fix up real evaluation (somehow)
and we have to have built into every topos, a class like:
class RewiredExponential[X, A, B](x:X, f: A => B): A > B

# Why I had to unroll the loops when computing action ideals

Here's why we can't currently construct the triadic topos.
The monoid T has order 8, and to construct the right ideals over it,
we have to construct "isIdeal" which tests for f: M -> Ω that:
∀ (s, t) in T x T, f(s) => f(st)
This involves ∀ing over T x T, i.e. constructing an arrow from Ω ^ (T x T) to Ω.
But the left hand star has 2 ^ 64 elements!

# structures for universal algebra  
Example: AlgebraicTheory()(*)(α * β := β * α)
This says: for the theory we're defining (commutative magmas) there are no constants, 1 binary operation \*, and one law.
How are the laws constructed/verified? A law is an equality between terms:
     α * β := β * α
All of these are Term\[Principal\] which means: they ultimately boil down to maps into the carrier 

# Going multivariate
We now assume our algebra A has various "supporting algebras" B, C, ... in its definition.
So we can express: an action of a monoid, a module over a ring.
Evaluation contexts: An EC keeps track of a multiproduct in a recursive typesafe way

- fix TODOs
- put optional scalars dot arg in a sensible place
- curry the constructor for AlgebraicTheory in tests... need an object helper?
- include optional "preassignment" symbol defs in an algebraic theory (e.g. II=x for monoids)
- TDD a NonNaiveMonoids
- sort out realRoot

# Wrappings API

Remind me again why this is needed? For ease of use writing tests.
Could we easily rewrite all the tests to do without it?
The PREARROW seems an intrinsically stupid idea, because if you
have an X => Y you should be able to construct an ARROW[X, Y].
So, start with: rewrite the FiniteSetsTests in a saner way? 
Is the DOT of FiniteSets a public object? Would it be that hard to
have tests that just used it directly?

# Deleting the old DiagrammaticTopos stuff and the wrapping layer

Why don't I do this? It has been pretty thoroughly shown to be
redundant.

# Topoi to implement

Topos of group actions: a useful exercise. Useful for permutations?

Topos of coalgebras: Too difficult for now. 
Need to understand theory better.
 
Topos of presheaves for an internal category: Too difficult.
At least the topos of coalgebras is a generalization (M & M explain)

Topos of sheaves for an L-T topology.
Do I need to implement machinery of Heyting algebras first?
The space of L-T topologies is a H-algebra ; actually it is the
nucleus of the H-algebra Omega. This is the same as the lattice of
congruences on Omega (check Matt F's paper).
Also understand: factorization of geometric morphisms.
They are always the composite of a sheaf embedding with... ?

Topos of slices: Useful exercise.
Use the McLarty construction, not the two-step M & M one.

Effective topos: Need to understand theory better.
Requires implementing the intuition of 'partial recursive functions'

Bumpables. Or more generally: Types S, T where we can tell if two
functions S => T are equal by their effect on the 'generic element'
of S. Is there a way to make this accessible from the type?
S.genericCompanion, something like that?

# Other ideas

Abstract Stone Duality?

Johnstone's notes on 'the schizophrenic character of 2' ?
Does the monad (_)^X help, with its sheaf algebras of n-cuboidal bands?

Diamond Theory. How geometric is it? Does any of it generalize?

Extract the idea of 'linkage' in the action topos code.
Get it straight for what pairs of types A, AA there is a linkage
A <-> AA. Probably not very many examples. Abstract.

# Permutation DSL

A bearable notation: val p = π(1,2,3)(4,5).π
But we then can't calculate p(5). Or can we?
More importantly, can we map p back to a sequence of cycles?
Do we need to extend the wrapping layer?
Or is there a need for interchangeable representations:
(sequence of cycles) <-> (dot in the topos)?
Seems there is a need for some kind of enhanced or inverse mapping.

# The Horror That Is AlgebraicMachinery

AlgebraicSort -> Principal, Scalar
    Should these classes actually do something?
    encapsulate principal/scalar processing inside them?
types Nullary/Unary/Binary/RightScalarBinaryOp
    Should these all be functional i.e. (X, X) => X? Not of type >{_,_}?
Law, NamedLaws
    ok
Term extends Dynamic - with ops *,**,***, etc
Operator
VariableTerm -> Principal/ScalarTerm
    ok
BinaryOpTerm, BinaryRightScalarOpTerm, BinaryScalarOpTerm, UnaryOpTerm
    ok. Define free variables for each
ConstantOperator -> Principal/ScalarConstant
    ok
OperatorAssignment
    :( has multiple methods for looking up different types of operator
    By default these all return None
    The subclasses AbstractBinaryOp, etc override these for some types
    Is there a better way to do it?
OperatorAssignments
    REALLY messy. Does essentially same lookup for each type
    ... factored out common code
    ... could be improved by formalizing operator types, i.e.
        having a class that encapsulates them?
        or 'operator context' that we can be in the context of?
AbstractBinaryOp/RightScalarBinaryOp/ScalarBinaryOp/UnaryOp
    Part of above mechanism: Selectively override the lookups.
    There MUST be a better way.
    Shouldn't each Operator come with its own type?
    i.e. it should be Operator[_ <: OperatorType]
    and then OperatorAssignment could use this type
StandardTermsAndOperators    
    Define things like o, II, α, β etc. Lookup for binops
AlgebraicTheory
    tell if an arrow is a morphism
    Algebra
        has an OperatorAssignment[_, _] < FIX
            Can make it an [S, T] ... OpAssign[T, S]*?
            Step 1: fix type safety for T
            Step 2: do for S. Have to adjust implementations of ":=" 
        EvaluationContext
            comes in type Simple, Compound
            ^ check overlap between these, factor out

EvaluationContext:
Factored out some of this.
It ends up being sensible to put the term-specific handling
in the base class, and then have the case statements in the
EvaluationContext implementations invoke these as necessary.
Would be better if:
you could invoke each term (uniformly) with the required 
ingredients and have it do this itself
Also lots of very similar methods here, 
should be able to refactor further

...so now only 3 areas that could be cleaned up:
- the cast in EvalContext: depends on recursive mechanism for
building the context from multiple dots. Is there a way round it?
- 

next step is:
add 'operator/term-type-specification classes' that localize
this machinery, so we don't need case splitting and 
multiple dispatch

