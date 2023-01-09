package com.fdilke.bewl2.apps

import com.fdilke.bewl2.sets.Sets.*
import com.fdilke.utility.Mask.*

object DebugPartialArrowClassifier extends App:

  val numbers: Set[Int] = (1 to 4).toSet

  println("The great task begins.")
  withDot(numbers) {
    println("And I can assure you it's well under way.")
    type D_OPTION[X] = DefaultOptionator.OPTION[X]
    type FOO = Int
    // val dotOptionFoo: Dot[D_OPTION[FOO]] = DefaultOptionator.partialArrowClassifier[FOO].classifier
    maskDot[FOO, Unit] {
      [F] => (_: Dot[F]) ?=> (_ : F =:= FOO) ?=> (_: FOO =:= F) ?=>
        // (actionF: groupD.Action[F]) => (_ : F =:= FOO) ?=> (foo2f : FOO =:= F) ?=>
        // FOO -- some ---> FOO*
        //  |    ^
        //  V   /
        // D_OPTION[FOO]
        val altPac: PartialArrowClassifier[F, D_OPTION[F]] = DefaultOptionator.partialArrowClassifier[F]
        given Dot[D_OPTION[F]] = altPac.classifier
        val altSome: F ~> D_OPTION[F] = altPac.some
        val f2foo: F ~> FOO = summon[F =:= FOO].substituteCo[[Z] =>> Z]
        val foo2f: FOO ~> F = summon[FOO =:= F].substituteCo[[Z] =>> Z]
        val extend: D_OPTION[F] ~> OPTION[FOO] = extendAlong[F, D_OPTION[F], FOO](altSome, f2foo)
        val extendInv: OPTION[FOO] ~> D_OPTION[F] =
          altPac.extendAlong[FOO, OPTION[FOO]](some[FOO], foo2f)
        assert {
          (extend o extendInv) =!= id[OPTION[FOO]]
        }
        val huberBubber: D_OPTION[F] ~> D_OPTION[F] = extendInv o extend
        println("size of dot for D_OPTION[F] is: " + dot[D_OPTION[F]].size)
        println("size of dot for OPTION[FOO] is: " + dot[OPTION[FOO]].size)
        println("isIso == " + huberBubber.isIso)
        assert {
          (extendInv o extend) =!= id[D_OPTION[F]]
        }
    }

  }

