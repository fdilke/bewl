package com.fdilke.bewl2.apps

import com.fdilke.bewl2.sets.{SetsUtilities}
import com.fdilke.bewl2.sets.SetsWithSlowActions
import SetsWithSlowActions.*
import SetsUtilities.*
import scala.language.postfixOps

// Investigating the 'card shuffle' property of FinSet: that in the group of permutations G = Aut(A x B),
// we have G = KHK where H (resp. K) is the subgroup of column (resp. row) preserving transformations.


object CardShuffle extends App:
  
  val startTime = System.currentTimeMillis()
  enum Suit:
    case Hearts, Clubs, Spades, Diamonds
  val numbers: Set[Int] = (1 to 3).toSet // (1 to 13).toSet

  println("The great task begins.")  
  withDot(numbers):
    withEnum[Suit]:
      withAutomorphismGroup[(Suit, Int), Unit]:
        [A] => (_ : Dot[A]) ?=> (group: Group[A]) ?=> (action: group.Action[(Suit, Int)]) =>
          println("Main group constructed.")  
          action.preserving(π0[Suit, Int]):
            [H] => (dotH : Dot[H]) ?=> (groupH: Group[H]) ?=> (embedH: H ~> A) =>
            action.preserving(π1[Suit, Int]):
              [K] => (dotK : Dot[K]) ?=> (groupK: Group[K]) ?=> (embedK: K ~> A) =>
              println(s"|H| = ${dot[H].size}, |K| = ${dot[K].size}, |A| = ${dot[A].size}")  
              println("Building khk.")  
              val khk: ((K, H), K) ~> A =
                case ((k, h), k2) =>
                (embedK(k) * embedH(h)) * embedK(k2)
              println("Verification under way...")  
              assert { khk.isEpic }
              println("Asserted")  

  val endTime = System.currentTimeMillis()
  val totalSec = (endTime - startTime)/1000
  println(s"Calculation completed ($totalSec sec).")
