import munit.FunSuite
import munit.Clue.generate

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class RattySpec extends FunSuite:
  test("example test that succeeds") {
    val obtained = 42
    val expected = 42
    val x: Future[Int] = Future {
      2
    }
// TODO: get -Yindent-colons to work    
//    val y: Future[Int] = Future:
//      2
    assertEquals(obtained, expected)
  }
