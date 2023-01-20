notes on a possibly 'deep' property of FinSet, deriving from the Hall matching theorem applied to a card trick

Suppose A, B are finite sets and we have a permutation π of A x B - abstracting the situation of a shuffled deck of cards.
(Could think of A as the 13 numbers, B as the 4 suits).

Then if the cards are arranged in a grid with 13 columns and 4 rows, we can pick a card in each column and pick out all numbers,
then do this again 3 times and arrive at another permutation of A x B which unpicks the original one. Spell out its properties.

do this for b = b1, b2, b3, b4
call the new function f

the card at column a, row b is π(a, b) <- A x B

then f(_, b1) picks out for each COLUMN, a ROW
so f: A x B -> B and

a => π(a, f(a, b)).π0 permutes A, say by phi(b) <- π(A)
so there is g(a, b) <- B with:

π(a, f(a, b)) = (phi(b)(a), g(a, b))

Do we know that f(a, -) is a permutation of B? Yes
So there is some kind of duality:
f(a, b) = chi(a)(b)

π(a, chi(a)(b)) = (phi(b)(a), g(a, b))    (*)
phi: B -> π(A)
chi: A -> π(B)
g: A x B -> B
but does g also have some sort of permutation living inside it?

Putting b' = chi(a)(b) we have b = chi_(a)(b')  (where chi_ is chi^-1) and
π(a, b') = ( phi(chi_(a)(b'))(a), g'(a, b') )
for a suitable new function g' sending (a, b') -> g(a, chi_(a)(b')) ... getting complicated

If we set π'(a, b) = π(a, chi(a)(b)), don't we already know that this must be a permutation of A x B? Yes

So could ask: what conditions on phi, chi, g ensure that π as defined above IS a permutation?
Probably g can't throw away information, and so must send (a, b) => xi(a)(b) for some yet-another-permutation map xi: A -> πB
but check. Condition is that the RHS of (*) is injective as a function on A x B, so misusing the capital letters:
  phi(b)(a) = phi(B)(A)   }
  g(a, b) = g(A, B)       } together imply a=A, b=B
At least we've finessed away chi, it is not part of the mix now. This appears intractable.
Except, the first equation forces b=B => a=A
Do we know from the above equation that g(a, _) is a permutation of B? Maybe not.
Can we not finesse away phi? Or tell something about the permutations of A over which this must range?
Does it help to make explicit the inverse of π, to which all the same reasoning applies - so we could dissect it, too?
Possibly there's a mangled version of g (say g') for which we can deduce that:
(a, b) -> (a, g(a, b)) is bijective ... but that would force g to split into a permutation  xi: A -> πB

later: can restore from any permutation of π(AxB) by permuting columns, then rows, then columns
so canonical surjection 
  (m!)^n x (n!)^m x (m!)^n => (mn)! 
Is this uniform? Arithmetically could it be?
No; for example if mn-1 is prime, the RHS doesn't divide the LHS, so it can't be uniform.

2!^3 = 8, 3!^2 = 36. So this is 8 * 36 * 8 = 2304 vs 6! = 720 ... factor of 3.
Or, 36 * 8 * 36 = 10368 ... factor of 14.
Can use Bewl to investigate this, given action on symmetry group.
Verify surjectivity, investigate uniformity.

(Discussion on Mathstodon about this)

The theorem can be rewritten as a statement about subgroups of S_mn.
If we let H =~= S_m^n be the subgroup of S_mn preserving rows, and
L =~= S_n^m the subgroup preserving columns, this says that S_mn = KHK = HKH,
so any double coset of H meets K and vice versa. You might say they're "entangled" or "complementary".

Good stretch goal for Bewl to verify all this using symmetric groups...done for low order (about 3x4)
