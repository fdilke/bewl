# Journey to category theory

- Journey to category theory
- decorated with crackly photos of dead mathematicians 

- Saunders MacLane (1909-2005)
- Coinventor of category theory
- Reception was hostile 
- People said it was too abstract

- book 'Categories for the Working Mathematician'
- I read it so you don't have to
- explains a series of concepts, culminating with Kan extensions in the
  last chapter. The master concept which encompasses everything
- The target audience: hard-nosed skeptics doing gritty, practical things

- Levels of math
- -1: The gritty, practical things (e.g. adding up bronze coins in the supermarket)
are calculations in workspaces whose names we largely don't even know (Z, Q, R, etc)
- 0: Mathematicians specializing in different types of workspace (monoids, groupe, etc)
- 1: theories of structure (model theory, universal algebra)
- 2: theories of theories (category theory)
- as with meta-object frameworks, we can now recursively abstract, and don't need
any more levels

- A typically hard-nosed, skeptical mathematician:
- Evariste Galois (1811-1832)
- A brilliant and very romantic figure
- Turned his life into a deleted scene from The Three Musketeers
- Got involved in post-Napoleonic revolutionary politics
- Was challenged to a duel 
- Died in a ditch age 21
- Poster boy for tragic early death
- This is not his story.

- Galois initiated the study of *fields*
- A field is an abstraction of the real number system R
- a system of numbers with 0, 1, +, -, *, /
- Another example: GF(5) = integers mod 5
- Small but perfectly formed: GF(5) = { 0_5, 1_5, 2_5, 3_5, 4_5 }
- 2 * 3 = 6 = 1, 1/2 = 3, 1/3 = 2, 1/4 = 4, 4 = -1
- There is a GF(q) for every prime power q = p ^ n
- Great for constructing codes, particularly with p = 2
- GF(4) is not the integers mod 4! Because 2 * 2 = 0
- Finite fields are fascinating, but this is not their story either.

- Let's try to construct a new field, starting with R
- R^N = R x R x R x ... 
- Not a field. Has 0, 1, +, -, * but not /
- because (1,0,0,0,...) can't have a multiplicative inverse
- Find an *equivalence relation* ~ such that R^N/~ is a field
- R^N/~ is just R/N with two sequences (a), (b) identified whenever (a) ~ (b)
- Obviously ~ has to play nice with +, -, *
~ Small matter of algebra, easily solved, but opens up a remarkable
rabbit hole.

When toys come alive

- Yes, there are many ways to construct such a ~.
- They only depend on N, not R or its algebraic structure
- We get a new field R* = R^N/~

- What is R* like?
- A (very) big brother to R
- The hyperreals
- Need a whole new vocabulary (model theory) just to describe the
relationship between R and R*
- R* is an *elementary extension*

- Precise sense in which it's a (very) big brother
- |R*| > |R|
- 'Transfer principle': A statement is true in R* <=> it's true in R
- So R* is ordered, has square roots precisely for numbers > 0, etc
- We are now in a head-on collision with ...

- Isaac Newton (1642-1726)
- Invented the calculus
- But unfortunately with a lot of mumbo-jumbo and hand-wavy stuff about 
"chimerical quantities" as limit operations weren't properly understood yet.
- It took centuries more to get all of that onto a sensible basis

- Richard Dedekind (1831–1916)
- Gave a proper description of the real numbers in 1888 

- R* has true infinitesimals - quantities lying strictly between 0 and any 
  positive real number
- The "dx, dy" of calculus brought to life
- Closed-form differentiation

- Can digitally remaster calculus
- Can do 'nonstandard analysis' - power series, contour integration and the
whole bag of tricks - with hyperreals. 
- There are books on SpringerLink about this

- How does the ~ work?
- Define a set U of subsets of N called an *ultrafilter*
- Define (a) ~ (b) if they coincide on a U-set
- Then R^N/~ has the same structure as R 

- We didn't use any field theory here.
- The construction works with any of a wide class of structures
- For any suitably conditioned theory of (say) widgets 
- we can extend any widget W to a bigger, badder mega-widget W*

- Don't even have to do this with the same widget W, N times.
- Can multiply up a bunch of widgets W_x => ℿ_U W_x 
- By tweaking U, we can genetically engineer the properties of the resulting 
ultraproduct widget, to order. 

- Jerzy Los (1920-1998)
- Ultraproduct theorem
- The "~" construction I'd stumbled on

- Now it gets paradoxical:

- A powerful construction, right? Actually no:
- "Ultraproducts are only useful for proving theorems about ultraproducts" 
(from the magisterial tome "Model Theory", Wilfrid Hodges)
- You can do equivalent constructions in a less mysterious way using "elementary" 
methods (e.g. Fraissé limits, which seem to me elementary only in a strict technical 
sense of not using any fancy set theory)

- What about ultrafilters?
- An Boolean operator (like AND or ?:, but infinitary) that commutes with all 
finitary Boolean operators
- Mental model: an ultrafilter on a set X is like something that can 
convincingly pretend to be an element of X
- A sort of abstract limit operation

- Seem to touch every area of mathematics
- Can prove Ramsey's theorem on graphs
- Can solve knotty problems in algebra (e.g. polynomial identity rings)
- Can digitally remaster topology, too

- There is a book about them on SpringerLink (Comfort and Negrepontis, 1974)
- I guarantee it won't leave you any the wiser.

- But: Existence proofs are non-constructive.
- That is, we can't actually construct one.
- Consolation prize: we almost can, using recalled brands of set theory 
(refounded on the Axiom of Determinacy)
- Are they like neutrinos, a fundamental particle almost too small to detect? 
- Even this intuition, I believe, is wrong.

- Ultrafilters and ultraproducts are a kind of blind spot, a deceptive illusion 
that reveals just how poorly we understand sets and set theory - the flawed 
foundation on which (most) math is built.
- This partly illuminates Gödel's results about how the axioms of set theory
are not fit for purpose, and never can be.
- Doing math with set theory is like using a camera whose pictures
consist almost entirely of lens flare
- Set theory is an annoying, legacy platform like MSDOS 2.0
- We can do better!
- Enter category theory and topos theory.



