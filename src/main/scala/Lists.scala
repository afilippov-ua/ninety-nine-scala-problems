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
}
