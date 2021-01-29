import Lists.{last, length, penultimate}
import org.scalatest.funsuite.AnyFunSuite

class ListsSuite extends AnyFunSuite {

  test("P01 - Getting the last element of a list has to return 8") {
    assert(last(List(1, 1, 2, 3, 5, 8)) == 8)
  }

  test("P01 - Getting the last element of an empty list has to throw an exception") {
    assertThrows[NoSuchElementException](last(List()))
  }

  test("P02 - Getting the penultimate element of a list has to return 5") {
    assert(penultimate(List(1, 1, 2, 3, 5, 8)) == 5)
  }

  test("P02 - Getting the penultimate element of an empty list has to throw an exception") {
    assertThrows[NoSuchElementException](penultimate(List()))
  }

  test("P04 - length of empty list has to return 0") {
    assert(length(List()) == 0)
  }

  test("P04 - length of a list with 1 element has to return 1") {
    assert(length(List(1)) == 1)
  }

  test("P04 - length of list with 6 elements has to return 6") {
    assert(length(List(1, 1, 2, 3, 5, 8)) == 6)
  }

}
