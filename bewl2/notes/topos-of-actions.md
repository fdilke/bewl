# Notes on the topos of actions

Start with the category Act-M of right actions by a monoid M.
0, 1, sums and products are streightforward.

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
