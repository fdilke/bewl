// exercise the topos of actions
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.fsets.{FiniteSets, FiniteSetsUtilities}

object Worksheet {
  private val (i, x, y) = ('i, 'x, 'y)
  val monoid = naiveMonoidFromTable(
    i, x, y,
    x, x, y,
    y, x, y
  )
  val topos = monoid.actions
  import topos._

  val regular = dot(monoid.regularAction)

//  (regular >> (omega > omega)) size

  (omega >> omega).size
}
