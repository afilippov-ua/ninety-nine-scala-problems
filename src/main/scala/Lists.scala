object Lists {

  def last[A](as: List[A]): A = as.last

  def penultimate[A](as: List[A]): A = as.takeRight(2).head
}
