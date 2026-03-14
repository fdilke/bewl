// exercise the topos of actions
import com.fdilke.bewl.fsets.FiniteSetsUtilities._
import com.fdilke.bewl.fsets.{FiniteSets, FiniteSetsUtilities}

object Worksheet {
  private val (i, x, y) = ('i, 'x, 'y)
  val monoid = monoidFromTable(
    i, x, y,
    x, x, y,
    y, x, y
  )
  val topos = FiniteSets.ToposOfMonoidActions of monoid
  import topos.omega

  val regular = dot(monoid.regularAction)

//  (regular >> (omega > omega)) size

  (omega >> omega).size
}
