package com.fdilke.bewl2.sets

import com.fdilke.bewl2.topos.PreTopos
import com.fdilke.bewl2.topos.PreToposWithDefaultToolkit
import com.fdilke.bewl2.topos.Topos
import com.fdilke.bewl2.topos.ProductMappable
import com.fdilke.bewl2.sets.SetsUtilities.*
import com.fdilke.utility.Shortcuts.*
import com.fdilke.bewl2.sets.morphenum.SetsMonoidAssistant
import com.fdilke.bewl2.sets.morphenum.{ FindPresentation, FindGenerators, ActionSplitter }
import scala.language.postfixOps

object PreSets extends PreToposWithDefaultToolkit[Set, [A] =>> A, Void, Unit, Boolean, Map]:
  override def equalArrows[X, Y](
    dotX: Set[X],
    dotY: Set[Y],
    f1: X => Y,
    f2: X => Y
 ): Boolean =
    dotX.forall { x =>
      f1(x) == f2(x)
    }

  override def uncachedProductObject[X, Y](
    dotX: Set[X],
    dotY: Set[Y]
  ): Set[(X, Y)] =
    for {
      x <- dotX
      y <- dotY
    } yield
      (x, y)

  override def uncachedExponentialObject[X, Y](
    dotX: Set[X],
    unitX: TOOLKIT[X],
    dotY: Set[Y],
    unitY: TOOLKIT[Y]
  ): Set[X Map Y] =
    allMaps(dotX, dotY).toSet

  override def sanityTest[X](
    dotX: Set[X]
  ): Unit = ()
  
  override def sanityTest[X, Y](
    dotX: Set[X],
    dotY: Set[Y],
    f: X ~> Y
  ): Unit =
    dotX.foreach { x =>
      if (!dotY.contains(f(x)))
        throw new IllegalArgumentException(s"target outside range: ${f(x)} not found in $dotY")
    }

  override val unitDot: Set[Unit] = Set(())
  override val zeroDot: Set[Void] = Set.empty
  override val omegaDot: Set[Boolean] = Set(true, false)
  override val truth: Unit => Boolean = _ => true

  override def enumerateMorphisms[X, Y](
    dotX: Set[X],
    unitX: TOOLKIT[X],
    dotY: Set[Y],
    unitY: TOOLKIT[Y]
  ): Iterable[X => Y] =
    allMaps(dotX, dotY)

  override def fromZero[X](dotX: Set[X]): Void => X = { _ =>
    throw new IllegalArgumentException("Encountered a VOID")
  }

  override def toUnit[X](
    dotX: Set[X]
  ): X => Unit = {
    _ => ()
  }

  override def evaluation[X, Y](
    dotX: Set[X],
    dotY: Set[Y]
  ): ((X Map Y, X)) => Y =
    (xMapY, x) => xMapY(x)

  override def transpose[X, Y, Z](
    dotX: Set[X],
    dotY: Set[Y],
    dotZ: Set[Z],
    xy2z: ((X, Y)) => Z
  ): X => (Y Map Z) =
    x => Map.from(
      dotY map { y =>
        y -> xy2z( (x, y) )
      }
    )

  override def doEqualizer[X, Y, RESULT](
    dotX: Set[X],
    dotY: Set[Y],
    f: X => Y,
    f2: X => Y
  )(
    block: [A] => RawEqualizer[A, X] => Set[A] => RESULT
  ): RESULT =
    block[X](
      new RawEqualizer[X, X] {
        override val inclusion: X ~> X =
          identity[X]

        override def restrict[R](
          dotR: Set[R],
          arrow: R ~> X
        ): R ~> X =
          arrow
      }
    )(
      dotX.filter { x =>
        f(x) == f2(x)
      }
    )

  override def chiForMonic[X, Y](
    dotX: Set[X],
    dotY: Set[Y],
    monic: X => Y
  ): Y => Boolean =
    y => dotX.exists { x =>
      monic(x) == y
    }

  override def backDivideMonic[X, Y, A](
    dotX: Set[X],
    dotY: Set[Y],
    dotA: Set[A],
    arrow: X => Y,
    monic: A => Y
  ): X => A =
    x => {
      val target: Y = arrow(x)
      dotA.find { a =>
        monic(a) == target
      } get
    }

class BaseSets extends Topos[
  Set, [A] =>> A, Void, Unit, Boolean, Map
](PreSets):
  def makeNullaryOperator[X: Dot](
    value: X
  ): Unit => X =
    _ => value

  def makeUnaryOperator[X: Dot](
    values: (X, X)*
  ): X => X =
    val map: Map[X, X] = Map[X, X](values: _*)
    if dot[X] != map.keySet then
      bail("incomplete or excessive unary operator definition")
    map

  def makeBinaryOperator[X: Dot](
    values: ((X, X), X)*
  ): ((X, X)) => X =
    val map: Map[(X, X), X] = Map[(X, X), X](values: _*)
    map

  def withMonoidFromTable[M, RESULT](
    table: M*
  )(
    block: Dot[M] ?=> Monoid[M] ?=> RESULT
  ): RESULT =
    val carrierSize = intSqrt(table.size)
    val carrierAsList = table.take(carrierSize)

    val mappings: Seq[((M, M), M)] =
      for {
        i <- 0 until carrierSize
        j <- 0 until carrierSize
      } yield (
        carrierAsList(i),
        carrierAsList(j)
      ) -> table(
        i * carrierSize + j
      )

    withDot(carrierAsList.toSet) {
      given Monoid[M] =
        Monoid[M](
          makeNullaryOperator[M](
            table.head
          ),
          makeBinaryOperator[M](
            mappings: _*
          )
        )

      block
    }

  override lazy val logicalOperations: LogicalOperations =
    new LogicalOperations:
      override val and: BiArrow[Boolean, Boolean, Boolean] = { _ & _ }
      override val implies: BiArrow[Boolean, Boolean, Boolean] = { !(_) | _ }
      override val or: BiArrow[Boolean, Boolean, Boolean] = { _ | _ }
      override val falsity: NullaryOp[Boolean] = { _ => false }

  override lazy val autoFinder: AutomorphismFinder =
    new AutomorphismFinder:
      override def withAutomorphismGroup[X : Dot, RESULT](
        block: [A] => Dot[A] ?=> (group: Group[A]) ?=> group.Action[X] ?=> RESULT
        ): RESULT =
        val seqX: Seq[X] = dot[X].toSeq
        val numbers: Seq[Int] = seqX.indices
        withDot[Seq[Int], RESULT](
          numbers.permutations.toSet[Seq[Int]]
        ) {
          implicit val group: Group[Seq[Int]] =
            Group[Seq[Int]](
              unit = makeNullaryOperator[Seq[Int]](numbers),
              multiply = { (p1: Seq[Int], p2: Seq[Int]) =>
                numbers map { s => p2(p1(s)) }
              },
              inverse = { (p: Seq[Int]) =>
                val array: Array[Int] = new Array[Int](seqX.size)
                numbers.foreach { s =>
                  array(p(s)) = s
                }
                array.toSeq
              }
            )
          val index: Map[X, Int] = Map(
            seqX.zip(numbers) :_*
          )
          given group.Action[X] = group.Action[X] { (x, seq) =>
            seqX(seq(index(x)))
          }
          block[Seq[Int]]
        }
        
  override lazy val epicVerifier: EpicVerifier =
    new EpicVerifier:
      override def isEpic[X: Dot, Y: Dot](
        arrow: X ~> Y
      ): Boolean =
        dot[X].map(arrow).size == dot[Y].size

  override val optionator: Optionator =
    new Optionator:
      override type OPTION[X] = Option[X]
      override def partialArrowClassifier[X: Dot]: PartialArrowClassifier[X, Option[X]] =
        withDot(
          dot[X].map(Some[X]) + None
        ) {
          new PartialArrowClassifier[X, Option[X]]:
            override val some: X ~> Option[X] =
              x => Some(x)
            override val none: Unit ~> Option[X] =
              _ => None
            override def extendAlong[V: Dot, W: Dot](
              monic: V ~> W,
              arrow: V ~> X
            ): W ~> Option[X] =
              w =>
                dot[V].find {
                  v => monic(v) == w
                }.map(arrow)
        }

object Sets extends BaseSets

object FastSets extends BaseSets 
  with SetsMonoidAssistant
  with FindGenerators
  with FindPresentation
  with ActionSplitter



  

  