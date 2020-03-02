package com.fdilke.bewl2.compacta

object CompactExponentialAlgo {
  def special[A](
    criterion: (A => Boolean) => Boolean
  ): Option[() => (A => Boolean)] =
    new CompactExponentialAlgoSpecial(criterion).result
}

class CompactExponentialAlgoSpecial[A](
  criterion: (A => Boolean) => Boolean
) {
  private class ExplorationNode[A](
    val arg: Option[A],
    val ifYes: Option[ExplorationNode[A]],
    val ifNo: Option[ExplorationNode[A]]
  ) { self =>
    def explore(): ExplorationNode[A] = {
      val position: ExplorationNode[A] = self
      val answer: Boolean =
        criterion { a =>
          true
        }
      position
    }

    def buildFunction(): A => Boolean =
      a =>
        ???
  }

  private var tree: ExplorationNode[A] =
    new ExplorationNode[A](None, None, None)

  while(true) {
    tree = tree.explore
  }

  val result: Option[() => (A => Boolean)] =
    if (true)
      Some(tree.buildFunction)
    else None
}

