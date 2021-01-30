import Lists._
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

  test("P05 - reverse a list [1,2,3,4,5] has to return list [5,4,3,2,1]") {
    assert(reverse(List(1, 2, 3, 4, 5)) == List(5, 4, 3, 2, 1))
  }

  test("P05 - reverse an empty list has to return an empty list") {
    assert(reverse(List()) == List())
  }

  test("P06 - has to return true for list [1,2,3,2,1]") {
    assert(isPalindrome(List(1, 2, 3, 2, 1)))
  }

  test("P06 - has to return true for list [1,2,3,3,2,1]") {
    assert(isPalindrome(List(1, 2, 3, 3, 2, 1)))
  }

  test("P06 - has to return false for list [1,2,3,3,4,2,1]") {
    assert(!isPalindrome(List(1, 2, 3, 3, 4, 2, 1)))
  }

  test("P06 - has to return true for list [1]") {
    assert(isPalindrome(List(1)))
  }

  test("P06 - has to return true for list [1,1]") {
    assert(isPalindrome(List(1, 1)))
  }
}
