package com.fdilke.bewl2.apps

import com.fdilke.bewl2.sets.{Sets, SetsUtilities}
import Sets.*
import SetsUtilities.*
import scala.language.postfixOps

// Investigating the 'card shuffle' property of FinSet: that in the group of permutations G = Aut(A x B),
// we have G = KHK where H (resp. K) is the subgroup of column (resp. row) preserving transformations.

enum Suit:
  case Hearts, Clubs, Spades, Diamonds

object CardShuffle extends App:

  val numbers: Set[Int] = (1 to 13).toSet

  println("The great task begins.")  
  withDot(numbers) {
    withEnum[Suit] {
      withEndomorphismMonoid[Suit, Unit] { [E] => (_: Dot[E]) ?=>
        (monoidSuit: Monoid[E]) ?=> (_: monoidSuit.Action[Suit]) ?=>
        withEndomorphismMonoid[Int, Unit] { [F] => (_: Dot[F]) ?=>
        (monoidInt: Monoid[F]) ?=> (_: monoidInt.Action[Int]) ?=>

        }
      }
    }
  }

  println("Aaaaand scene.")
