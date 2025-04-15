package com.fdilke.bewl2.topos

import com.fdilke.bewl2.topos.Topos
import munit.FunSuite
import scala.language.postfixOps
import java.io.File
import com.fdilke.utility.Mask._

abstract class GenericToposSpec[
  DOT[_],
  CTXT[_],
  VOID,
  UNIT,
  BEWL,
  >[_, _]
](
 val topos: Topos[DOT, CTXT, VOID, UNIT, BEWL, >]
) extends FunSuite:

  import topos.*

  type FOO
  type BAR
  type BAZ

  case class IsomorphismSituation[S: Dot, T: Dot](
    isomorphism: S ~> T
  ):
    def sanityTest: Unit =
      isomorphism.sanityTest
      if (!isomorphism.isIso)
        throw new IllegalArgumentException("not an isomorphism!")
    def withIsomorphism[Z](
      block: [_S, _T] => Dot[_S] ?=> Dot[_T] ?=> (_S ~> _T) => Z
    ): Z =
      block[S, T](isomorphism)

  case class EqualizerSituation[S: Dot, M: Dot, T: Dot](
    r: S ~> M,
    s: M ~> T,
    t: M ~> T
  ):
    def sanityTest: Unit =
      topos.sanityTest(r)
      topos.sanityTest(s)
      topos.sanityTest(t)
      if (s =!= t)
        throw new IllegalArgumentException("equalizing two arrows that are already equal!")
      assert:
        (s o r) =!= (t o r)

    def apply[RESULT](
      capture: [S, M, T] => (
        r: S ~> M,
        s: M ~> T,
        t: M ~> T
      ) => (
        Dot[S], Dot[M], Dot[T]
      ) ?=>
        RESULT
    ): RESULT =
      capture(r, s, t)

  trait ToposFixtures:
    val foo2bar: FOO ~> BAR
    val foo2baz: FOO ~> BAZ
    val foobar2baz: (FOO, BAR) ~> BAZ
    val monicBar2baz: BAR ~> BAZ
    val foo2ImageOfBar: FOO ~> BAZ
    val equalizerSituation: EqualizerSituation[_, _, _]
    val isomorphismSituation: IsomorphismSituation[_, _]

  def withTestDots(
     block: Dot[FOO] ?=> Dot[BAR] ?=> Dot[BAZ] ?=> ToposFixtures => Unit
  ): Unit

  withTestDots(runTests)

  private def runTests(
    fixtures: ToposFixtures
  )( using
    Dot[FOO],
    Dot[BAR],
    Dot[BAZ]
  ): Unit =
    import fixtures._

    test("identity arrows have sane equality semantics"):
      assert:
        identity[CTXT[FOO]] =!= id[FOO]
      assert:
        id[FOO] =!= id[FOO]

    test("identity arrows obey composition laws"):
      assert:
        (foo2bar o id[FOO]) =!= foo2bar
      assert:
        (id[BAR] o foo2bar) =!= foo2bar

    test("sane fixtures"):
      sanityTest[FOO]
      sanityTest[BAR]
      sanityTest[BAZ]
      sanityTest(foo2bar)
      sanityTest(foobar2baz)
      sanityTest(monicBar2baz)
      sanityTest(foobar2baz)
      sanityTest(foo2ImageOfBar)

    test("biproduct diagrams work"):
      sanityTest[(BAR, BAZ)]
      val productArrow: FOO ~> (BAR, BAZ) = 
        foo2bar x foo2baz
      productArrow.sanityTest

      π0[BAR, BAZ].sanityTest
      π1[BAR, BAZ].sanityTest

      assert:
        (π0[BAR, BAZ] o productArrow)  =!= foo2bar
      assert:
        (π1[BAR, BAZ] o productArrow) =!= foo2baz
      val recombine: (BAR, BAZ) ~> (BAR, BAZ) =
        π0[BAR, BAZ] x π1[BAR, BAZ]
      assert:
        recombine =!= id[(BAR, BAZ)]

    test("distinguishes projection arrows"):
      assert:
        π0[FOO, FOO] !=!= π1[FOO, FOO]

    test("caches products"):
      val fooXbar1: Dot[(FOO, BAR)] =
        summon
      val fooXbar2: Dot[(FOO, BAR)] =
        summon
      assert:
        (fooXbar1.asInstanceOf[Object]) eq (fooXbar2.asInstanceOf[Object])

    test("the unit object behaves"):
      sanityTest[UNIT]
      val fooTo1: FOO ~> UNIT =
        toUnit[FOO]
      fooTo1.sanityTest

      val arrows: Iterable[FOO ~> UNIT] = morphisms[FOO, UNIT]
      assert: 
        arrows.size == 1
      assert: 
        arrows.head =!= fooTo1 
      assert:
        (toUnit[BAR] o foo2bar) =!= fooTo1

    test("the zero object behaves"):
      sanityTest[VOID]
      val barFrom0: VOID ~> BAR =
        fromZero[BAR]
      barFrom0.sanityTest

      val arrows: Iterable[VOID ~> BAR] = morphisms[VOID, BAR]
      assert: 
        arrows.size == 1
      assert: 
        arrows.head =!= barFrom0
      assert:
        (foo2bar o fromZero[FOO]) =!= barFrom0

    test("has enumeration of arrows"):
      extension [X: Dot, Y: Dot](arrow: X ~> Y)
        def appears: Unit =
          assert:
            morphisms[X, Y] exists { _ =!= arrow }
      (foo2bar         appears)
      (foo2baz         appears)
      (foobar2baz      appears)
      (monicBar2baz    appears)
      (foo2ImageOfBar  appears)
      morphisms[FOO, BAR].foreach:
        _.sanityTest
      morphisms[FOO, BEWL].foreach:
        _.sanityTest

    test("can factorize arrows into 'monic o epic'"):
      for { arrow <- morphisms[FOO, BAR] }
        arrow.factorize:
          [I] => (_: Dot[I]) ?=> (epic: FOO ~> I, monic: I ~> BAR) =>
          assert:
            epic.isEpic
          assert:
            monic.isMonic
          assert:
            arrow =!= ( monic o epic )

    test("consistently calculates arrows from the initial to the terminal"):
      assert:
        toUnit[VOID] =!= fromZero[UNIT]

    test("can compute inverses"):
      isomorphismSituation.sanityTest
      isomorphismSituation.withIsomorphism:
        [S, T] => (_: Dot[S]) ?=> (_: Dot[T]) ?=> (isomorphism: S ~> T) =>
          val theInverse: T ~> S =
          isomorphism.inverse
          assert:
            theInverse.isIso
          assert:
            (theInverse o isomorphism) =!= id[S]
          assert:
            (isomorphism o theInverse) =!= id[T]

    test("can construct exponential diagrams"):
      sanityTest[BAR > BAZ]
      val eval: (BAR > BAZ, BAR) ~> BAZ  =
        evaluation[BAR, BAZ]
      sanityTest(eval)
      val foo2bar2baz: FOO ~> (BAR > BAZ) =
        transpose(foobar2baz)
      sanityTest(foo2bar2baz)

      // reconstruct the arrow from its transpose:
      // f(foo, bar) === eval(f*(foo), bar)
      assert: 
        foobar2baz.=!=(
          (cFooBar: CTXT[(FOO, BAR)]) =>
            val cFoo: CTXT[FOO] = π0[FOO, BAR](cFooBar)
            val cBar: CTXT[BAR] = π1[FOO, BAR](cFooBar)
            val fnBarBaz: CTXT[BAR > BAZ] = foo2bar2baz(cFoo)
            eval(fnBarBaz, cBar)
        )

    test("caches exponentials"):
      val fooToBar1: Dot[FOO > BAR] = summon[Dot[FOO > BAR]]
      val fooToBar2: Dot[FOO > BAR] = summon[Dot[FOO > BAR]]
      assert:
        (fooToBar1.asInstanceOf[Object]) eq (fooToBar2.asInstanceOf[Object])

    test("calculates equalizers"):
      equalizerSituation[Unit]:
        [S, M, T] => (
          r: S ~> M,
          s: M ~> T,
          t: M ~> T
        ) => (
          ss: Dot[S], mm: Dot[M], tt: Dot[T]
        ) ?=>
          s.?=(t):
            [A] => (aa: Dot[A]) ?=> (equalizer: Equalizer[A, M]) =>
              val e = equalizer.inclusion
              assert:
                (s o e) =!= (t o e)
              assert:
                (e o equalizer.restrict(r)) =!= r

    test("has a subobject classifier"):
      sanityTest[BEWL]
      sanityTest(truth)
      sanityTest(falsity)
      val char: BAZ ~> BEWL = 
        monicBar2baz.chi
      sanityTest(char)
      assert:
        (char o monicBar2baz) =!= toTrue[BAR]
      val restriction: FOO ~> BAR = 
        foo2ImageOfBar \ monicBar2baz
      restriction.sanityTest
      assert:
        (monicBar2baz o restriction) =!= foo2ImageOfBar
    
    test("overrides the logops driver correctly, if at all"):
      if !(logicalOperations.isInstanceOf[DefaultLogicalOperations]) then
        val defaultLogOps: LogicalOperations = new DefaultLogicalOperations
        val heyting: HeytingAlgebra[BEWL] =
          new HeytingAlgebra[BEWL](
            defaultLogOps.falsity,
            truth,
            defaultLogOps.and,
            defaultLogOps.or,
            defaultLogOps.implies
          )
        mask[BEWL, HeytingAlgebra, Unit](heyting): 
          [ALTBEWL] => (altHeyting: HeytingAlgebra[ALTBEWL]) =>
            (_ : ALTBEWL =:= BEWL) ?=> (_ : BEWL =:= ALTBEWL) ?=>
            val reallyBewl: ALTBEWL ~> BEWL =
              summon[ALTBEWL =:= BEWL].substituteCo[CTXT]
            given Dot[ALTBEWL] =
              summon[BEWL =:= ALTBEWL].substituteCo[Dot]:
                summon[Dot[BEWL]]
            given HeytingAlgebra[ALTBEWL] = altHeyting
            assert:
              heytingAlgebras.isMorphism:
                reallyBewl

    test("overrides the monic verifier correctly, if at all"):
      if !(monicVerifier eq DefaultMonicVerifier) then
        def checkMonics[A: Dot, B: Dot]: Unit =
          morphisms[A, B].foreach: arrow =>
            assert:
              arrow.isMonic == DefaultMonicVerifier.isMonic(arrow)
        checkMonics[FOO, BAR]
        checkMonics[BAR, FOO]
        checkMonics[FOO, FOO]
        checkMonics[BEWL, BEWL]

    test("overrides the epic verifier correctly, if at all"):
      if !(epicVerifier eq DefaultEpicVerifier) then
        def checkEpics[A: Dot, B: Dot]: Unit =
          morphisms[A, B].foreach: arrow =>
            assert:
              arrow.isEpic == DefaultEpicVerifier.isEpic(arrow)
        checkEpics[FOO, BAR]
        checkEpics[BAR, FOO]
        checkEpics[FOO, FOO]
        checkEpics[BEWL, BEWL]

    test("overrides the automorphism finder correctly, if at all"):
      if ! (autoFinder eq DefaultAutomorphismFinder) then
        autoFinder.withAutomorphismGroup[FOO, Unit]:
          [A] => (_ : Dot[A]) ?=> (group: Group[A]) ?=> (actionA: group.Action[FOO]) ?=>
          DefaultAutomorphismFinder.withAutomorphismGroup[FOO, Unit]:
            [D] => (_ : Dot[D]) ?=> (groupD: Group[D]) ?=> (actionD: groupD.Action[FOO]) ?=>
              val groupIso: D ~> A = 
                actionD.toExponent \
                  actionA.toExponent
              assert:
                groups.isMorphism:
                  groupIso
              assert:
                groupIso.isIso
              mask[FOO, groupD.Action, Unit](
                actionA.induced(groupIso)
              ):
                [F] => (actionF: groupD.Action[F]) =>
                  (f2foo : F =:= FOO) ?=> (foo2f : FOO =:= F) ?=>
                  given Dot[F] = 
                    foo2f.substituteCo[Dot]:
                      summon[Dot[FOO]]
                  val realFoo: F ~> FOO = 
                    f2foo.substituteCo[CTXT]
                  given groupD.Action[F] = actionF
                  assert:
                    realFoo.isIso
                  assert:
                    groupD.actions.isMorphism:
                      realFoo

    test("overrides the optionator correctly, if at all"):
      if ! optionator.isInstanceOf[DefaultOptionator.type] then
        type D_OPTION[X] = DefaultOptionator.OPTION[X]
        maskDot[FOO, Unit]:
          [F] => (_ : Dot[F]) ?=> (_ : F =:= FOO) ?=> (_ : FOO =:= F) ?=>
            val altPac: PartialArrowClassifier[F, D_OPTION[F]] =
              DefaultOptionator.partialArrowClassifier[F]
            given Dot[D_OPTION[F]] =
              altPac.classifier
            val f2foo: F ~> FOO =
              summon[F =:= FOO].substituteCo[CTXT]
            val foo2f: FOO ~> F =
              summon[FOO =:= F].substituteCo[CTXT]
            val extend: D_OPTION[F] ~> OPTION[FOO] =
              extendAlong[F, D_OPTION[F], FOO](
                altPac.some, 
                f2foo
              )
            val extendInv: OPTION[FOO] ~> D_OPTION[F] =
              altPac.extendAlong[FOO, OPTION[FOO]](
                some[FOO],
                foo2f
              )
            assert:
              extend.isIso
            assert:
              extendInv.isIso
            assert:
              (extend o extendInv) =!= id[OPTION[FOO]]
            assert:
              (extendInv o extend) =!= id[D_OPTION[F]]
