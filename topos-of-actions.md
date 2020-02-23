# Notes on the topos of monoid actions.

This is a simple example of a "Cantorian variety" (my terminology), 
i.e. an algebraic theory with unary and join operators satisfying the conditions of
one of the theorems in Johnstone's paper about varieties that are topoi.
The theorem gives necessary (but not sufficient) conditions.
Briefly, the join operators have to be "formally bijective" (reversible)
and there have to be equations like

    u(J(...)) ::== JJJ(u'(...), u''(...), ...)
    
for every u and J, where JJJ is a composite join operator, and the expressions
u''(...) operate on values taken from the arguments of J on the LHS.
    
A more interesting example of a Cantorian variety is the topos of
Jonsson-Tarski (aka Cantor) algebras, discussed separately. This sets the stage.

## Monoids and actions

A *monoid* is a set M with a distinguished element 1 and a binary operator * with:

    - a * 1 = 1 * a = a
    - a * (b * c) = (a * b) * c
    
A (right) action over M is then a set A with another binary operation A x M => A,
also written *, such that:

    - m * 1 = m
    - (m * a) * b = m * (a * b)
    
# Actions over M form a topos

We show the class M-Set of right actions has:

    - Finite products
    - Equalizers
    - A subobject classifier
    - Exponentials
    
The first two are very straightforward. The unit (terminal) M-action is just the
one-element set 1 with trivial action. Products are defined componentwise.
Given f, g: A => B then the subset of A where f, g coincide is a subaction.

To motivate the subobject classifier and exponential, consider M as a right action
over itself. Then it's a (cyclic) generator, also an element classifier:

    for an M-action A, M-maps M => A are interchangeable with elements of A.
    
Applying this to a candidate subobject classifier Ω, we find that elements of Ω
are interchangeable with maps M => Ω, and so with subactions of M, i.e.
right ideals of M.

So define Ω to be the set of right ideals of M, with this multiplication:

    for a right ideal R <= M, and m in M,
    R * m = { n: m * n in R } <= M.
    
You can verify this satisfies the monoid action laws. 
How are charactertistic arrows defined?

Given a monic arrow S => A of M-actions, consider A as a subaction of B,
and define the characteristic arrow x_S: A => Ω by:

    x_S(a) = { m: a * m in S } <= M.
    
You can verify this is an M-map, and that this yields an appropriate
pullback diagram for any monic S => A.

Similarly for exponentials. Given a candidate B^A, elements of B^A are interchangeable
with maps M => B^A, and so with maps M x A => B.

So we define B^A = Hom(M x A, B), with an action specified by

    for f: M x A => B and m in M,
    (fm)(n, a) ::== f(mn, a)
    
You can verify that fm in B^A and that this does define an action.
Evaluation is defined by:

    ev: B^A x A => B
    for f: M x A => B and a in A,
    ev(f, a) = f(1, a)
    
You can verify that this defines an M-map. Finally, given h: X x A => B,
we transpose it to h': X => B^A by

    h'(x)(m, a) ::== h(xm, a)
    
You can verify that this is an M-map and that it factors h through ev:

    h(x, a) = ev(h(x), a).
    
That's it: M-Set is now a topos :) QED                 