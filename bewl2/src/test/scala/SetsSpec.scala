import com.fdilke.bewl2.Topos
import munit.FunSuite
import com.fdilke.bewl2.sets.Sets.implicitTopos

class SetsSpec extends GenericToposTests[Set]

abstract class GenericToposTests[
  SET[_]: Topos
] extends FunSuite:
  test("example test that succeeds") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }
