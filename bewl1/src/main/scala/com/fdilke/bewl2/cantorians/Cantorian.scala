package com.fdilke.bewl2.cantorians

import java.util.function.Supplier

import com.fdilke.bewl2.cantorians.Catcher.applyCatcher
import com.fdilke.bewl2.topology.Compact

import scala.annotation.tailrec
import scala.collection.Iterator.iterate
import scala.language.postfixOps

class GroundedCatcher[T, U](
  val either: Either[U, T => GroundedCatcher[T, U]]
) extends CatcherFType[GroundedCatcher[T, U], T, U]

object GroundedCatcher {
  implicit def catcherTude[T, U]: Catcher[GroundedCatcher[T, U], T, U] =
    CatcherFType.standardCatcher[GroundedCatcher[T, U], T, U](e => new GroundedCatcher(e))
}

trait Cantorian extends PitcherFType[Cantorian, Boolean] with Function[Int, Boolean] { cantorian =>
  def asIterable: Iterable[Boolean] =
    new Iterable[Boolean] {
      override def iterator: Iterator[Boolean] =
        iterate(
          cantorian
        ) {
          _.tail
        }.map {
          _.head
        }
    }
  @tailrec
  final def apply(index: Int): Boolean =
    if (index == 0)
      head
    else
      tail(index - 1)

  def take(n: Int): Seq[Boolean] =
    if (n == 0)
      Seq.empty
    else
      head +: tail.take(n - 1)

  def drop(n: Int): Cantorian =
    iterate(
      cantorian
    ) {
      _.tail
    }.drop(n) next

  def slice(from: Int, until: Int): Seq[Boolean] =
    drop(from).take(until - from)
}

object Cantorian {
  def apply(
    h: => Boolean,
    t: => Cantorian
  ): Cantorian =
    new Cantorian {
      override def head: Boolean = h
      override def tail: Cantorian = t
    }

  def cycle(values: Boolean*): Cantorian =
    new Supplier[Cantorian] { supplier =>
      override def get: Cantorian =
        values
          .foldRight[Supplier[Cantorian]](
            supplier
          )((b: Boolean, c: Supplier[Cantorian]) => () => Cantorian(b, c.get)) get
    } get

  implicit val cantorianPitcher: Pitcher[Cantorian, Boolean] =
    new Pitcher[Cantorian, Boolean] {
      override def head(
        cantorian: Cantorian
      ): Boolean =
        cantorian.head

      override def tail(
        cantorian: Cantorian
      ): Cantorian =
        cantorian.tail

      override def construct(
        head: => Boolean,
        tail: => Cantorian
      ): Cantorian =
        Cantorian(head, tail)
    }

  // can we make 'compactness' implicit instead and not need this?
  implicit val cantorianCompactness: Compact[Cantorian] =
    Pitcher.compactness[Cantorian, Boolean]

  implicit val cantorianJonssonTarski: JonssonTarski[Cantorian] =
    new JonssonTarski[Cantorian] {
      override def join(l: Cantorian, r: Cantorian): Cantorian =
        Cantorian(
          l.head,
          Cantorian(
            r.head,
            join(
              l.tail,
              r.tail
            )
          )
        )

      override def left(join: Cantorian): Cantorian =
        Cantorian(
          join.head,
          left(
            join.tail.tail
          )
        )

      override def right(join: Cantorian): Cantorian =
        Cantorian(
          join.tail.head,
          right(
            join.tail.tail
          )
        )
    }
}
