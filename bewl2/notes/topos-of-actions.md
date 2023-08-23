# Notes on the topos of actions

Start with the category Act-M of right actions by a monoid M.
0, 1, sums and products are straightforward.

## The object of truth

This is the set of right ideals of M, made into an action by 'left backdivision':

  I.m = I\m := { n <- M: mn <- I }    (verify this is an action)

The truth map 1 -> RightIdeals just picks out the whole ideal M.
Then the characteristic map A ~> RightIdeals for a subaction B <= A is defined by
  a -> B\a := { n <- M: an <- B }     (verify this is an M-map)

## Exponentials

Three-point plan for defining exponentials in Act-M:

1. Given A, B then the exponential B^A is defined to be Hom_M(M x A, B) made into an action by:
  for p: M x A ~> B then (pm)(n, a) := p(mn, a)     (verify this is an action)

2. Define evaluation, eval: B^A x A ~> B by:
  eval(p, a) := p(1, a)                             (verify this is an M-map)

3. Given f: X x A ~> B define its transpose f*: X ~> B^A by:
  f*(x)(n, a) := f(xn, a)                           (verify this is an action)

Then you can verify that f can be reconstructed from f* via evaluation,
also that the transpose of evaluation is the identity on B^A

## Theory behind the Morphism Enumerator

Morphism Enumeration

Theorem:
Let A be a finitely generated action over a monoid.
Then any monogenic subaction is contained in a maximal monogenic subaction,
and the set of these is finite, say { Mi }; their union is A.
Let Gi be the set of generators of Mi.
Then the minimal generating sets of A are precisely the transversals of the family { Gi }. 
In particular, they all have the same size.

(This gives a vector space-like theory of dimension for finitely generated actions).

Proof. Let A = union of Ma_j, j ranging over some finite index set J.
Then any monogenic subaction Ma is contained in one of the Ma_j, so if it’s maximal,
it must actually be one of these. Hence their number is finite.
Given a monogenic Ma <=Ma_j, we can find a maximal Ma_k containing Ma_j 
(since there are only finitely many to choose from) and then this must be maximal monogenic.

Next, the G_i are disjoint because each g in G_i generates and so determines M_i. 
Clearly any transversal T = { t_i } generates all of A, and no proper subset of T does.
But any generating set must meet G_i for each i and so include a transversal. []

This means that any reasonable (e.g. greedy) algorithm for finding generators of an action
will find one of minimal size.

The idea of the Morphism Enumerator is to precalculate for every action, a presentation -
generators and relators - and then use this to enumerate mappings between actions more efficiently.
The relators are just generators of the action defined by a congruence on the original action,
resulting from the implicit mapping from a free action defined by the generators.
So we can use the same algorithm to calculate those efficiently.

(I suspect that my current algo, which goes to the trouble of calculating all the maximal
monogenics, could be replaced by a much simpler one which just looped over the elements
removing anything redundant. Plan on eventually doing this for Bewl II.)
We also factor each action into "connected components" in the obvious way.

This is just an irreducible representation of it as a coproduct.


