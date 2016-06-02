sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def tail[A](x: List[A]): List[A] = x match {
    case Nil => Nil
    case Cons(_, xs: List[A]) => xs
  }

  def setHead[A](x: List[A], another: A) = x match {
    case Nil => Nil
    case Cons(_, xs: List[A]) => Cons(another, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = if (n == 0) l else drop(tail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x))
        dropWhile(xs, f)
      else
        Cons(x, xs)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, Cons(y, ys)) => Cons(x, init(ys))
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight2(as, (b: B) => b)((a: A, bf: B => B) => (bb: B) => bf(f(bb, a)))(z)

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft2(as, (b: B) => b)((bf: B => B, a:A) => (bb: B) => bf(f(a, bb)))(z)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def product(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, x) => x + 1)

  def sum[A](as: List[Int]) = foldLeft(as, 0)(_ + _)

  def reverse[A](as: List[A]) = foldLeft2(as, Nil: List[A])((x, xs) => Cons(xs, x))

  def append[A](as: List[A], ass: List[A]): List[A] = foldRight(as, ass)(Cons(_, _))

  def flatten[A](ass: List[List[A]]): List[A] = foldRight2(ass, Nil: List[A])(append)
}

println(List.length(List(1, 2, 3, 4)))

println(List.reverse(List(1, 2, 3, 4)))

println(List.append(List(1, 2, 3, 4), List(1, 2, 3, 4)))

println(List.flatten(List(List(1, 2, 3, 4), List(1, 2, 3, 4), List(1, 2, 3, 4))))
