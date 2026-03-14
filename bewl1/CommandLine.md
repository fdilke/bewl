# The Bewl command line

You can manipulate categorical objects and arrows from the command line, which
may be illuminating if you're trying to learn category theory.

I describe some of the Bewl syntax here - see the code if you want to do more!

Because the word "object" is overused in programming, I refer to "dots and arrows"
instead of "objects and arrows" which most textbooks use. We're still doing category theory!

# Running Bewl

To run Bewl, you will need __git__ and __sbt__. (On OS X, use MacPorts or Brew to install these.)

    git clone https://github.com/fdilke/bewl.git
    cd bewl
    sbt console

# Importing classes to work with finite sets

Let's start with the topos of finite sets. These imports will help:

    import com.fdilke.bewl.fsets._
    import FiniteSets._
    import FiniteSetsUtilities._

Before creating our own dots and arrows, let's look at the built-in ones.

# The built-in dots and arrows

There is a dot called `I` (corresponding to the terminal object called `1` in category theory).

In the topos of sets, it's just a one-element set.

There is another dot called `omega` (representing the truth object `{ true, false }`) and an

arrow called `truth`, which goes from I to omega.

So these tests return true (that's ordinary Boolean true):

    truth.source == I
    truth.target == omega

You can also look at all these objects just by typing their names at the command line -
the type annotations may be illuminating.

The truth arrow is monic, but it isn't epic:

    truth.isMonic
    truth.isEpic

give true and false respectively.

You can compose it with the appropriate identity arrows:

    (truth o I.identity) == truth
    (omega.identity o truth) == truth

(you need the brackets because of Scala's precedence rules).

# Creating dots and arrows

Let's create a dot that might represent a set of techies who work at Springer:

    val techies = dot("Felix", "Artur", "Tom")

and an arrow from it to omega that might express membership of a team:

    val inTeam = arrow(techies, omega)("Felix" -> true, "Artur" -> true, "Tom" -> false)

You can verify that this is epic, but not monic.

# Subobjects

Because the arrow `inTeam` has target omega, it represents a subobject of `techies` which we can extract:

    val team = inTeam.whereTrue

Note the declared type of this object is an EQUALIZER which gives you a hint how it was constructed,

as the equalizer of `inTeam` with `truth o techies.toI` ; these are both arrows from `techies` to `I`.

`whereTrue` here actually expands to the equalizer `inTeam ?= truth o techies.toI`.

Every equalizer comes with an inclusion arrow, which is always monic:

    team.inclusion.source == team
    team.inclusion.target == techies
    team.inclusion.isMonic

You can check it's not epic in this case.

Another utility available on arrows into omega is to test if they're true everywhere.
This method is called `toBool` because it essentially turns the arrow into a Boolean truth value.

So `inTeam.toBool` is `false`, but `(truth o techies.toI).toBool` is `true`.

# Isomorphisms

Let's construct an isomorphism between `team` and `omega`:

    val myIso = arrow(team, omega)("Felix" -> false, "Artur" -> true)

You can verify this is iso:

    myIso.isIso

and even construct its inverse:

    myIso.inverse

which unsurprisingly maps `false` to `Felix` and `true` to `Artur`.

Just to prove this really is an inverse:

    (myIso o myIso.inverse) == omega.identity
    (myIso.inverse o myIso) == team.identity

# Note on types

Bewl uses the Scala type system to enforce consistency on arrows. In our example,

- `techies` is a DOT[String]
- `omega` is a DOT[Boolean]
- the arrow `inTeam`: `techies` -> `omega` is an ARROW[String, Boolean]

and you can only compose arrows whose types match up.

You can also only use whereTrue on arrows whose target is omega.

# Enumerating arrows

Given two dots, there is a way to list all the arrows from one to the other:

    for (arrow <- team >> omega) println(arrow)

or to just count them:

    (team >> omega).size

You can verify that there's only ever one arrow from anywhere to `I`.

# Functional notation for arrows

You can also construct arrows using a functional notation, and you can 
apply them as if they were functions. This essentially makes arrows
interchangeable with functions, except that an arrow knows its source
and target, and can be tested for equality with other arrows.

Let's construct another dot to illustrate this:

    val three = dot(1, 2, 3)

    val even = arrow(three, omega)(1 -> false, 2 -> true, 3 -> false)

So `even: three -> omega` just tells whether numbers are even. 
We could define it another way:

    val even2 = three(omega) { x => (x % 2) == 0 }

    even == even2

which shows they are the same arrow. You can apply these to numbers directly:

    even(3)

will return a boolean value of false.

(Note that all this works in an arbitrary topos, where there is still a concept of
elements even though the dots are not sets and don't have members!)

In particular, we can now calculate the identity another way:

    techies.identity == ( techies(techies) { x => x } )

# Products

Let's construct some more dots and some products of them:

    val fruits = dot('apple, 'pear, 'banana)
    val sizes = dot(1, 2)
    val fruitSizes = fruits x sizes

The printout suggests that `fruitSizes` consists of ordered pairs from `fruits` and `sizes`.
The product comes with a couple of projection operators `π0`, `π1` :

(Note on OS X you can get a `π` by typing Alt-P)

    fruitSizes.π0.source == fruitSizes
    fruitSizes.π0.target == fruits
    fruitSizes.π1.source == fruitSizes
    fruitSizes.π1.target == sizes

It's also possible to multiply arrows: if `p: X -> fruits` and
`q: X -> sizes`, then `p x q` is an arrow `X -> fruits x sizes`.

These work as you'd expect. In particular, we can multiply the two
projection arrows together and get the identity:

    (fruitSizes.π0 x fruitSizes.π1) == fruitSizes.identity

You can also verify that the product arrows always compose nicely
with the projection arrows to give the original components of the product.

# Exponentials

There is also an "exponential dot" representing all the arrows from a specified dot to another:

    val myExponential = fruits > sizes

Like an arrow, this has a source and target:

    myExponential.source == fruits
    myExponential.target == sizes

This comes with an 'evaluation' function which is an arrow from the product
`myExponential x fruits` to `sizes`. It just evaluates functions:
Conceptually, `evaluation(f, x)` is just `f(x)`.

There is also a `transpose` method that turns any arrow `X x fruits -> sizes`
into an arrow into the exponential, `X -> fruits > sizes`. This is just like
currying a function.

You can convince yourself that these all fit together properly and that you can
reconstruct any arrow from its transpose using the evaluation arrow (the functional
notation is useful here).

# Beyond the topos of sets

Bewl's `GenericToposTests` suite tests that all these product and exponential constructions
work together as they're supposed to, not just for sets but in any topos.

# To be continued

More soon!