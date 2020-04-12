# To explore

- Matchers: all(_), sameElementsAs(), loneElement
- Rudin delta, Stokes. Vector Calculus book on SpringerLink
- Racket
- Emacs
- Light table for Clojure if it's still an active project
- Haskell / Idris - proofs
- Solidity
- Akka / streams
- Lurie talk about categorization of Fourier
- scalaz, cats, cats-effect. Shapeless
- macros. paradise library? Also Dotty macros
- Can port [scala-workflow](http://www.cse.chalmers.se/~evgenyk/papers/scala-workflow.pdf)?
    This seems to correspond to Idris' "idiom brackets"
- Caching in Idris: It might be possible to have a purely functional SortedTree structure that did caching.    
- Mazzolla book
- Schwarz-Christoffel

 # ScalaFmt
 
 how to disable formatting:
 
     // format: off
     val identity = Array(1, 0, 0,
                          0, 1, 0,
                          0, 0, 1)
     // format: on

may be necessary with AlgebraicMachineryTest which is somewhat complex...
this fixed it     

did better with minimal .scalaafmt.conf from the scalafmt project itself

note very complete config options available from Cmd-comma preferences / scalafmt tab

to search GitHub:

    https://github.com/search?utf8=%E2%9C%93&q=filename%3A.scalafmt.conf&type=Code
    
# Comagmas - this needs writing up

These are the opposite of magmas, which are just sets with a binary operation which 
needn’t have a 1, or be associative or commutative or satisfy any nice algebraic laws. 
So a comagma is just a set X with a structure map:

     X -> X + X

It turns out these structures definitely deserve a place in the world of Bewl and 
“cantorian music theory” or whatever the rapidly burgeoning body of knowledge is to 
be called, because:

(1) Cantorians (maps N -> Bool) form a comagma K.

(2) For any other comagma X, there’s a unique morphism of comagmas X -> K. So actually K is the terminal object of the category of comagmas. 

(3) it turns out this category is actually a topos!

Here’s why: Consider the free monoid M on 2 generators. 
Then you can convince yourself that a comagma is just a right action of M together 
with an M-map to K, which of course has a natural right action structure. 
So the category of comagmas is just a slice category of the category of 
right M-actions, so is a topos. 

(4) A slight modification of this argument may make the category of reversible
comagmas (i.e. ones whose structure map is iso) into a topos, also with the 
Cantorian comagma K as its 1. I haven’t quite checked all the details, but 
this seems plausible. 

(5) For any reversible comagma X, and any set A, A ^ X is a Jónsson-Tarski algebra.
