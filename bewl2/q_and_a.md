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
- support for biproducts, so given X and Y for which there's SET[_],
  we can generate a 'product kit' consisting of SET[(X, Y)],
  C[(X, Y)] -> C[X] and C[Y], plus the ability to multiply arrows
- maybe require C[_] to have Map... so there's IdentityWithMap?

# type convolutions:
how do we constrain a type CTXT so that a CTXT[A] always has def map[B](f: A => B): CTXT[B] ?
SetOps[A, Set, Set[A]] shows the way... persevere with this;
  trait Mappable[A, CTXT[_] /* <: Mappable[A, CTXT]] */ ] { self: CTXT[A] =>
    def map[B](f: A => B): CTXT[B]
  }
but it's no good. Need a typeclass.

# Dot classes
A monad in the case of Set. Also MonoidAction[M, _] ? Not really.