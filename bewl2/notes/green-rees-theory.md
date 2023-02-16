# About GreenRees.scala with its algorithms for idempotent monoids

Algorithms based on this paper: https://www.math.ucla.edu/~pak/hidden/Courses/Lothaire-Ch2.pdf

This shows that the free idempotent monoid on finitely many generators is free.

I'd conjecture a higher level structure theory, specifically: 
the stronger statement that the theory of i-monoids is finitely generated, i.e. there is a
"primordial" _finite_ i-monoid of which every i-monoid is involved in some power.
This would 'explain' the local finiteness of idempotent monoids, by this result:

https://math.stackexchange.com/questions/3972017/how-to-show-that-that-every-finitely-generated-variety-is-locally-finite

How would one prove this? What is the primordial? Could Bewl help find it?
Is there a property of monads here waiting to be extracted?
Btw, do we know that every monad on FinSet is strong? Is there a quick proof?
What kind of topos property is this?

what are the monoliths in this variety? Guessing they have a simple structure.

One could ask also if idempotent monoids have 'enough injectives': there's a universal algebra
result characterizing this (need congruence extension property, etc etc) so maybe not.

I believe there's a simple way to generate a 'canonical form' which standardizes and simplifies the
calculations. and may even produce 'a shortest' word representing each equivalence class.
Rather than try and prove this, I'll first see if it works empirically.

later: Yes. But what you really need is a canonical form that is also lexicographically earliest.
Left with a bit of a residue of not-so-successful formas and calculations.

Rename all that to GascoignePeesXxx as an interesting experiment
Start again with something based on String rather than Seq[H], because
Strings are not really interchangeable with Seq[Char] and it's a pain
also: we need H to be orderable and that's a pain too
Pick out the successful bits of that. Maybe have canonical1 as the original,
not-best algorithm, develop a better canonical in stages

Also I believe this is what the Green theory's eggbox diagram will
look like for IM(n) the free idempotent monoid on n letters:
For finite monoids D = J ; for IM(n) this is the set of letters in a word.
So IM(n) has 2^n D-classes.
Within each D-class it only depends on the set of letters, say there are k of them
So we have E(k) in each H-class and there are B(k)^2 of them, B(k) being the
number of L- (or equivalently R-)classes. (E for egg, B for box)
So we have another equation:

|I(n)| = (horrid sum) = SUM_k=0^n { E(k) * B(k)^2 }
where the formulae for E and B are probably not too bad.
Something else I should model

