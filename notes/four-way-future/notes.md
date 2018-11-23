# contents...

- think: credible speaker, content / analysis / refs / social proof 
- feel: fun, helpful, relevant, eye-opening
- do: explore FP

- decode: FP-aware, unclear benefits, CT opaque
- encode:
- timing: 20 min, paced, right amount of maths
- polite: I found these techniques helpful
- create message:

## content

    reassuringly little math
    monads, strong monads, diagrams
    CT group experiment: glass "It's a ******* product"

    how to start??
    Worth listening to because...? MC introduces??
    audience persona: FP/Scala-interested devs 

\newpage

- my project, four different languages
- Java: fast, hacky, lots of typecasts, inadequately expressive (lambdas etc)
- Clojure: :) expressive, macros, interactive, good dev exp; :( homoiconic, not typesafe, SLOW
- Scala: good fit, idiomatic, DSL-friendly, forces you to think properly about types.
    Nightmares with abstract types, type constraints, but now it's fine. Thru pain barrier
- beyond Scala: Idris / Racket because they can...

\newpage

$$ I = \int \rho R^{2} dV  \infty \mathbb{ E } $$ page!?

\newpage

each

\newpage

---

# Bewl 

## a programming language for topos theory

more precisely, a Scala DSL 
for the Mitchell-Benabou internal language of a topos, 
with some topos implementations

---

## Topos implementations (2)

The last three all work inside an existing topos.

So if E is a topos, we can construct a new topos
Aut(E) consisting of all the dots-with-a-single-
automorphism in E. 

Similarly, if M is a monoid object in E, we can
construct the topos of all objects A in E that 
come with an action of M, i.e. an arrow
A x M > A subject to the algebraic laws.

Exponentials, the subobject classifier, logical 
operations on truth values, etc will all be 
computed automatically. 

---

## Sample helper methods

Calculating the "name" of an arrow (see McLarty):
```
    trait Arrow[S <: ~, T <: ~] ... {
        ...    
        final lazy val name: UNIT > (S → T) =
          (source > target).transpose(I) {
              (i, x) => arrow(x)
          }
    }
```
From the same class, a method to tell if an arrow is epic:
```
    final lazy val isEpic: Boolean =
      target.exists(source) {
        (t, s) => target.=?=(
          t, arrow(s)
        )
      } toBool
```
 
---