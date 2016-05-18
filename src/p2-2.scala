def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  as match {
    case Array() => true
    case Array(_) => true
    case Array(a0, a1, _) => {
      def f(a0: A, a1: A, rest: Array[A]): Boolean = {

        if (ordered(a0, a1)) {
          rest match {
            case Array() => true
            case _ => f(a1, rest.head, rest.tail)
          }
        } else {
          false
        }
      }
      f(as(0), as(1), as.tail.tail)
    }
  }
}
