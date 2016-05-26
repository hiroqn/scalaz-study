sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def tail[A](x: List[A]): List[A] = x match {
    case Nil => Nil
    case Cons(_, xs: List[A]) => xs
  }

  def setHead[A](x: List[A], another: A) = x match {
    case Nil => Nil
    case Cons(_, xs: List[A]) => Cons(another, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = drop(tail(l), n - 1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x: A, xs: List[A]) =>
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
}

val x = List(1, 2, 3, 4, 5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101

}

print(List.tail(List(1, 2, 3, 4, 5)))

