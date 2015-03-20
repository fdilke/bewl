# The Bewl command line

You can manipulate categorical objects and arrows from the command line, which
may be illuminating if you're trying to learn category theory.

I describe some of the Bewl syntax here - see the code if you want to do more!

Because the word "object" is overused in programming, I refer to "dots and arrows"
instead of "objects and arrows" which most textbooks use. We're still doing category theory!

# Running Bewl

To run Bewl, you will need git and sbt. (On OS X, use MacPorts or Brew to install these.)

```
git clone https://github.com/fdilke/bewl.git
cd bewl
sbt console
```

# Importing classes to work with finite sets

Let's start with the topos of finite sets. These imports will help:

```
import com.fdilke.bewl.fsets._
import FiniteSets._
import FiniteSetsUtilities._
```

Before creating our own dots and arrows, let's look at the built-in ones.

# The built-in dots and arrows

There is a dot called `I` (corresponding to the terminal object called `1` in category theory).

In the topos of sets, it's just a one-element set.

There is another dot called `omega` (representing the truth object `{ true, false }`) and an

arrow called `truth`, which goes from I to omega.

So these tests return true (that's ordinary Boolean true):

```
truth.source == I
truth.target == omega
```

The truth arrow is monic, but it isn't epic:

```
truth.isMonic
truth.isEpic
```

give true and false respectively.

You can compose it with the appropriate identity arrows:

```
truth.o(I.identity) == truth
omega.identity.o(truth) == truth
```

(In compiled Scala, you don't need so much punctuation: `truth o I.identity`.)

# Creating dots and arrows

Let's create a dot that might represent a set of techies who work at Springer:

```
val techies = dot("Felix", "Abdul", "Tom")
```

and an arrow from it to omega that might express membership of a team:

```
val inTeam = arrow(techies, omega, "Felix" -> true, "Abdul" -> true, "Tom" -> false)
```

You can verify that this is epic, but not monic.

# Tricks with dots and arrows

Because the arrow `inTeam` has target omega, it represents a subobject of `techies` which we can extract:

```
val team = inTeam.whereTrue
```

Note the declared type of this object is an EQUALIZER which gives you a hint how it was constructed,

as the equalizer of `inTeam` with `truth o techies.toI` ; these are both arrows from `techies` to `I`.

Let's construct an isomorphism between `team` and `omega`:

```
val myIso = arrow(team, omega, "Felix" -> false, "Abdul" -> true)
```

You can verify this is iso, and even construct its inverse:

```
myIso.isIso
myIso.inverse
```

which unsurprisingly maps `false` to `Felix` and `true` to `Abdul`.
