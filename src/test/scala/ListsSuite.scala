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

  test("P06_2 - has to return true for list [1,2,3,2,1]") {
    assert(isPalindrome2(List(1, 2, 3, 2, 1)))
  }

  test("P06_2 - has to return true for list [1,2,3,3,2,1]") {
    assert(isPalindrome2(List(1, 2, 3, 3, 2, 1)))
  }

  test("P06_2 - has to return false for list [1,2,3,3,4,2,1]") {
    assert(!isPalindrome2(List(1, 2, 3, 3, 4, 2, 1)))
  }

  test("P06_2 - has to return true for list [1]") {
    assert(isPalindrome2(List(1)))
  }

  test("P06_2 - has to return true for list [1,1]") {
    assert(isPalindrome2(List(1, 1)))
  }

  test("P07 - flatten has to return list [1, 1, 2, 3, 5, 8]") {
    assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
  }

  test("P08 - compress function has to return list ['a', 'b', 'c', 'a', 'd', 'e']") {
    assert(compress(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) == List('a', 'b', 'c', 'a', 'd', 'e'))
  }

  test("P09 - pack function has to pack repeated elements in a separate sublists") {
    assert(pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')) ==
      List(List('a', 'a', 'a', 'a'), List('b'), List('c', 'c'), List('a', 'a'), List('d'), List('e', 'e', 'e', 'e')))
  }


}
