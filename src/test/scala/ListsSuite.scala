import Lists.last
import org.scalatest.funsuite.AnyFunSuite

class ListsSuite extends AnyFunSuite {

  test("Getting the last element of a list has to return 8") {
    assert(last(List(1, 1, 2, 3, 5, 8)) == 8)
  }

  test("Getting the last element of an empty list has to throw an exception") {
    assertThrows[NoSuchElementException](last(List()))
  }

}
