# test framework?
using munit, for laughs. 
seems ok except asserions a bit clunky - might use helpers

# is there a way to not have braces in tests? 
or a test framework that is more Scala 3 friendly?
work out exact rules with indentations, lambdas
compiler option -Yindent-colons is your friend...?
this does in fact work, you just have to put up with red marks

# conditions on the 'context' wrapper type?

initially thought this would have to be a monad, or support map,
but all we need is:

- support for arrows C[X] -> C[Y]
- support for biproducts, so given X and Y for which there's DOT[_],
  we can generate a 'product kit' consisting of DOT[(X, Y)],
  C[(X, Y)] -> C[X] and C[Y], plus the ability to multiply arrows
- maybe require C[_] to have Map... so there's IdentityWithMap?

# type convolutions:
how do we constrain a type CTXT so that a CTXT[A] always has def map[B](f: A => B): CTXT[B] ?
DOTOps[A, DOT, DOT[A]] shows the way... persevere with this;
  trait Mappable[A, CTXT[_] /* <: Mappable[A, CTXT]] */ ] { self: CTXT[A] =>
    def map[B](f: A => B): CTXT[B]
  }
but it's no good. Need a typeclass.

# Dot classes
A monad in the case of DOT. Also MonoidAction[M, _] ? Not really.

# should try this:
  extension (x: C) def pair(y: C) = (x, y)
also anon context parameters:
  def maximum[T](xs: List[T])(using Ord[T]): T =
    xs.reduceLeft(max)

# upgrade implicits
  as described here: https://dotty.epfl.ch/docs/reference/contextual/relationship-implicits.html
  
# eq
  to apply this to types DOT[...], I had to cast them to Object ; and then presumably
  Scala tests whether they have the same address

# Polymorphic function types e.g.
  [A] => List[A] => List[A]
  https://dotty.epfl.ch/docs/reference/new-types/polymorphic-function-types.html
  
Seems helpful, and maybe an optimization, to add "UNIT" to the laundry list of
type arguments to a topos. Could argue for → (the exponent type) similarly.
What about VOID, and the zero stuff generally, which we can infer?
Maybe better to do this as an exercise and make sure it's isomorphic to the built-in?
also add it as a fallback and calculate the from-scratch version anyway, AND have a 
mechanism where these can be separated.
