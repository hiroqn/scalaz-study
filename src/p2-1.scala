def fib(n: Int): Int = {
  @annotation.tailrec
  def fibAcc(a: Int, b: Int, n: Int): Int = {
    n match {
      case 0 => a
      case _ => fibAcc(b, a + b, n -1)
    }
  }
  fibAcc(0, 1, n)
}
