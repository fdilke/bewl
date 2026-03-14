# Bewl

For programmers, the Bewl DSL looks like a kind of new programming language, 
implemented within Scala.
Like Haskell, it is 'pure' (without side effects).
There are still types and functions, but:

* given any two functions that take an A and return a B, we can compare them for equality
* The types A, B, etc represent abstract collections over which values in the respective types can range.
It's best to think of the types as real objects, and the things ranging over them as temporary figures of
speech which have no existence outside the scope of auxiliary methods used to manipulate them.

This already has the same sort of weirdness as quantum physics, but moreover:

* there is a special 'truth type' called TRUTH which replaces the usual type Boolean
* there are truth values other than the usual *true* and *false*
* as a consequence of this, there is no 'if-then-else' (how would it deal with a value that was
only partly true?)
* But there are still AND, OR and NOT (although NOT has been slightly mangled in translation) and we can
still quantify over all of these strange-sounding values, and the results obey a conditioned
subset of ordinary logic.

Some of the topoi implemented in Bewl do have the ordinary two-valued concept of truth.
These are called Boolean topoi. Sets, permutations and group actions are examples.

It's about 2 years ago that I implemented the first non-Boolean topos. This seemed quite exciting because it was a
 chance to explore the world of non-Boolean truth values, but we'll see soon that there's a catch.
The topos I implemented is the topos of actions over a monoid, which are a bit like simplified vector spaces
in which there is no addition.

To explain what a monoid is, here is an example of creating one in Bewl:

```scala
val monoidOf3 =
      monoidFromTable(
        i, x, y,
        x, x, y,
        y, x, y
      ) // right-dominant on two generators
```

This defines a little self-contained multiplication table on a set with only three elements, { *i*, *x*, *y* }.
For example, you can see that *x* * *y* is *y*. Actually, both *x* and *y* are *right-dominant*,
in that they swamp anything that is multiplied by them on the left: *a* * *x* = *x* for all *a*,
 and similarly for *y*.

You can also see that *i* behaves like a 1, in that *i* * *a* = *a* * *i* = *a* for any *a*.
An algebraist would call *i* a *unit element* and say that this defines a monoid of order 3.
It's not obvious, but you can verify that the *associative law* holds for this system: for any
*x*, *y* and *z* we have *x* * (*y* * *z*) = (*x* * *y*) * *z*. This law, and the unit law requiring
 *i* to preserve everything, comprise the complete set of requirements for a monoid.

Let's call this monoid M. A *right action* of M is another set A whose elements can be multiplied on the
right by elements of M, in such a way that the obvious algebraic laws hold: *a* *i* = *a*, and
*a* (*m* * *n*) = (*a* *m*) *n* for any *a* in A and *m*, *n* in M.

This might seem abstruse, but the world is full of monoids and actions. If you consider the positive
real numbers, they form a monoid under multiplication, and then one-, two- and three-dimensional
space are all examples of actions over that monoid.

Another example is to take a two-element monoid, even simpler than the one above, consisting of just
+1 and -1 under multiplication. An action for this monoid is just a set with an involutive symmetry,
which you can think of as a vertical reflection onto itself, like a Rorschach blot.

Yet another, more abstract example takes any object A with any reasonable structure whatsoever, and
consider the set M of all mappings from A to itself which preserve that structure. Then M is a monoid,
and A is an action of M. This turns out to be a useful way to describe all kinds of structures, because M
encodes all sorts of insights into the structure of A.

For any monoid M, the actions of M form a *topos*, which means that they behave a lot like sets -
there are concepts of addition, multiplication, exponentiation and truth-values that behave like
those for sets - and the machinery of Bewl becomes applicable to them, as soon as there is an
implementation of the topos of actions for that monoid.

My first implementation was very complex - it used a lot of extra wrapping classes to get around type
restrictions in Scala. More seriously, it was also very slow. The complexity of the code didn't help,
but the real problem was the difficulty of computing exponentials.

For any two actions A and B, Bewl has a concept of the 'exponential object' B ^ A which represents
all mappings from A to B.

For the topos of actions over a monoid M, it turns out that under the covers this exponential
object is really the set of all mappings from M x A to B, where M is the monoid itself regarded as an
action over itself, the multiplication on ordered pairs in the product action M x A acts componentwise,
and we only consider mappings which respect the right multiplication of M.

Now unless at least one of A and B are completely trivial, this is quite a lot of computation. We have to
look at every conceivable mapping from a product set to somewhere else, and then loop over all possible
multiplications to check the algebraic laws are preserved.

This was unworkably slow, and meant that I could only just run a test suite for the tiny three-element
monoid I described explicitly by its multiplication table above. As it was, several tests had to be further
simplified or elided. (Programmers will be glad to know that Bewl is entirely test-driven, and there is a
generic test suite for checking topos implementations.)

But now there is a plan for simplifying and speeding up all of this.

The first step was to simplify the code by getting rid of the all the type adapter layers and associated machinery.
This required getting over a mental stumbling block which I'll attempt to describe: In Bewl, it appeared a good
idea to have the product type A x B have elements (instances) which really were ordered pairs. This seemed to
work so well, and to represent a tighter integration of the DSL with the language, that I applied it to
exponential (function) objects too. But closer analysis reveals this as a misstep.

The idea was that elements ranging over the function type B ^ A should really be functions A => B. This had some
immediate benefits for programming convenience, but for the topos of actions, it turns out that you need to
construct objects which act in this way at two different levels (because an element of B ^ A doesn't "really" map
A => B, it maps M x A => B), and the types got confused. Hence the need for additional wrapping and machinery.

In the end I found a trick whereby the user can still have his cake and eat it: the exponential elements
are no longer functions, but there is a mechanism using Scala's implicit conversions that allows you to pretend they
still are functions, almost all of the time. This seems an acceptable compromise.

With that in place, the actions code could be simplified. Now to speed it up. Given a monoid action A, my idea
is to express the multiplication table of A in a compact form (via 'generators') that make it easy to
describe mappings from A to any other monoid. Once you know where the generators are sent, the images of all the
other elements are then implied automatically.

Using the vector space analogy, to map a two-dimensional space (coordinated by x- and y- axes, say) into somewhere
else, you only need to specify the destinations of the two unit vectors in the x- and y-directions. The destinations
of all the other vectors then follow by linearity.

The first step was to prove a theorem expressing a simple and efficient algorithm for finding generators of a monoid
action; these are like the bases of a vector space, which act as alternate coordinate systems. It turns out, rather
surprisingly, that every finite monoid action has its bases all the same length. In other words, there is a well-defined
concept of dimension for monoid actions (although it does depend on finiteness conditions being imposed).

Given two generators g, h for an action, you can then consider all pairs (m, n) of the monoid for which gm = hn. These
pairs form a monoid action of their own, so we can use the algorithm again to find generators for the set of pairs.
These 'second-generation' generators may be called *relators*. Once these are assembled, there is then a final
'winnowing' step to get rid of any redundant relators already implied by previous ones, which uses standard textbook
algorithms to efficiently analyze equivalence relations.

Together, all the generators and relators form a complete description of the monoid action, and better yet,
they can be used to efficiently enumerate its arrows into any other action, as follows:

Suppose we want to find all M-preserving mappings from M x A to B. We first find generators and relators for M X A. Then
a recursive backtracking algorithm can construct all mappings from the set generators to B, building up partial
mappings, and at each stage needing only to check a small number of relators are satisfied.

There is already a literature on computing "presentations" (as such generator/relator descriptions are called)
for groups, and the popular computer algebra package GAP has library routines to support them. Presentations for
monoids and their actions seem to be less well covered, so perhaps I am breaking new ground here.

Anyway, I've now started implementing this, which has meant learning more about dependent types in Scala, and
brushing up some principles of algorithm design. So far, the results are promising!

# Misc

For many people, the theory of fields gives a dramatic feeling of depth, because we are for the first time explaining
 and enriching a deeply familiar structure, driven only by a curiosity to extend the range of the language.

Frankly, the context of Bewl is so abstract that it is quite difficult to describe the mechanics of it even to
programmers or mathematicians unfamiliar with topos theory. The underlying ideas are quite simple - the difficulty
is just in the rather daunting amount of abstraction needed to get there.

It seems absurd to point out that when adding up a shopping list, one is implicitly working
with algebraic structures based in the category of sets. Yet we do this kind of thing all the time,
perceiving the machineries that make up the world around us.
