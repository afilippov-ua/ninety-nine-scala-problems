import scala.annotation.tailrec

object Lists {

  def last[A](as: List[A]): A = as.last

  def penultimate[A](as: List[A]): A = {
    @tailrec
    def go(cur: List[A], prev: List[A]): A = {
      if (cur == Nil) {
        prev.head
      } else {
        go(cur.tail, prev.tail)
      }
    }

    go(skip(as, 2), as);
  }

  @tailrec
  def skip[A](as: List[A], n: Int): List[A] = {
    if (n == 0 || as.isEmpty)
      as
    else
      skip(as.tail, n - 1);
  }

  def nth[A](n: Int, as: List[A]): A = skip(as, n).head

  def length[A](as: List[A]): Int = {
    @tailrec
    def go[A](s: List[A], n: Int): Int = s match {
      case h :: t => go(t, n + 1)
      case Nil => n
    }

    go(as, 0);
  }

  def reverse[A](as: List[A]): List[A] = {
    @tailrec
    def go(head: List[A], tail: List[A]): List[A] = head match {
      case Nil => tail
      case h :: t => go(t, h :: tail)
    }

    go(as, Nil)
  }

  // Time complexity - O(N)
  // Space complexity - O(1) - if we don't consider recursive stack
  // possible improvement is to break in the middle of the list
  def isPalindrome[A](as: List[A]): Boolean = {
    def go(head: List[A], tail: List[A]): (List[A], Boolean) = {
      if (head != Nil) {
        val tuple = go(head.tail, tail)
        if (!tuple._2) {
          (Nil, false)
        } else {
          if (head.head == tuple._1.head) {
            (tuple._1.tail, true)
          } else {
            (Nil, false)
          }
        }
      } else {
        (tail, true)
      }
    }

    go(as, as)._2
  }

  // Super simple solution with linear time and space complexity
  def isPalindrome2[A](as: List[A]): Boolean = {
    reverse(as) == as
  }

  def flatten(as: List[Any]): List[Any] = as match {
    case (x: List[Any]) :: xs => flatten(x) ::: flatten(xs)
    case head :: tail => head :: flatten(tail)
    case Nil => Nil
  }

  def compress[A](as: List[A]): List[A] = as match {
    case a1 :: a2 :: tail if (a1 == a2) => compress(a1 :: tail)
    case head :: tail => head :: compress(tail)
    case Nil => Nil
  }

  def pack(as: List[Any]): List[List[Any]] = {
    def go(as: List[Any], acc: List[Any]): List[List[Any]] = as match {
      case a :: tail if acc.isEmpty => go(tail, a :: acc)
      case a :: tail if a == acc.head => go(tail, a :: acc)
      case a :: tail if a != acc.head => acc :: go(tail, List(a))
      case Nil => List(acc)
    }
    go(as, List())
  }

  def encode(as: List[Any]): List[(Int, Any)] = {
    pack(as)
      .map(lst => (lst.size, lst.head))
  }
}
