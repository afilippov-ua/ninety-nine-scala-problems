import Lists.{last, penultimate}
import org.scalatest.funsuite.AnyFunSuite

class ListsSuite extends AnyFunSuite {

  test("P01 - Getting the last element of a list has to return 8") {
    assert(last(List(1, 1, 2, 3, 5, 8)) == 8)
  }

  test("P01 - Getting the last element of an empty list has to throw an exception") {
    assertThrows[NoSuchElementException](last(List()))
  }

  test("P02 - Getting the penultimate element of a list has to return 8") {
    assert(penultimate(List(1, 1, 2, 3, 5, 8)) == 5)
  }

  test("P02 - Getting the penultimate element of an empty list has to throw an exception") {
    assertThrows[NoSuchElementException](penultimate(List()))
  }

}
