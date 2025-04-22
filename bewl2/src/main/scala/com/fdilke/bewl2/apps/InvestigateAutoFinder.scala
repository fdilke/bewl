package com.fdilke.bewl2.apps

import com.fdilke.bewl2.sets.Sets
import com.fdilke.bewl2.topos.Topos
import Sets.{Automorphism => Permutation}
import com.fdilke.utility.Mask._

object InvestigateAutoFinder extends App:
  val Ʒ: Topos[Sets.Automorphism, [A] =>> A, Void, Unit, Boolean, Map] =
    Sets.toposOfAutomorphisms
  type FOO = Symbol
  val Seq(a, b, c, d) : Seq[FOO] =
    Seq("a", "b", "c", "d").map:
      Symbol(_)
  
  val fooPermutation: Permutation[FOO] =
    Sets.withDot(Set(a, b, c, d)):
      Permutation[FOO]:
        Map(a -> c, b -> d, c -> a, d -> b)

  Ʒ.withDot(
    fooPermutation
  ):  
    Ʒ.DefaultAutomorphismFinder.withAutomorphismGroup[FOO, Unit]:
      [D] => (_ : Ʒ.Dot[D]) ?=> (groupD: Ʒ.Group[D]) ?=> (actionD: groupD.Action[FOO]) ?=>
      Ʒ.autoFinder.withAutomorphismGroup[FOO, Unit]:
        [A] => (_ : Ʒ.Dot[A]) ?=> (groupA: Ʒ.Group[A]) ?=> (actionA: groupA.Action[FOO]) ?=>
        import Ʒ.RichArrow
        println(s"default group size = ${groupD.dot.dot.theDot.dot.size}")
        println(s"autoFinder group size = ${groupA.dot.dot.theDot.dot.size}")
          assert:
            actionD.toExponent.isMonic
          assert:
            actionA.toExponent.isMonic
          val groupIso: D => A =
            actionD.toExponent \
              actionA.toExponent
          groupIso.sanityTest
          assert:
            Ʒ.groups.isMorphism:
              groupIso
          assert:
            groupIso.isIso
          mask[FOO, groupD.Action, Unit](
            actionA.induced(groupIso)
          ):
            [F] => (actionF: groupD.Action[F]) =>
              (f2foo : F =:= FOO) ?=> (foo2f : FOO =:= F) ?=>
              given Ʒ.Dot[F] =
                foo2f.substituteCo[Ʒ.Dot]:
                  summon[Ʒ.Dot[FOO]]
              val realFoo: F => FOO =
                f2foo
              realFoo.sanityTest
              given groupD.Action[F] = actionF
              assert:
                realFoo.isIso
              assert:
                groupD.actions.isMorphism:
                  realFoo
