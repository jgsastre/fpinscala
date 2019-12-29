package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil           => sys.error("tail of empty list")
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil           => sys.error("setHead on empty list")
    case Cons(_, tail) => Cons(h, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else
      l match {
        case Nil           => Nil
        case Cons(x, tail) => drop(tail, n - 1)
      }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  def init[A](l: List[A]): List[A] = ???

  def length[A](l: List[A]): Int = ???

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def map[A, B](l: List[A])(f: A => B): List[B] = ???
}

object Exercise31 {

  def main(args: Array[String]): Unit = {
    import List._

    println(s"The value of x if $List.x")
  }
}

object Exercise32 {

  def main(args: Array[String]): Unit = {

    def tailTest[A](l: List[A], expected: List[A]) = {
      println(
        s"For list $l its tail is ${List.tail(l)} and expected ${expected}"
      )
    }

    tailTest(List("1", "2", "3"), List("2", "3"))
    tailTest(List(1, 2, 3, 4), List(2, 3, 4))
  }
}

object Exercise34 {

  def main(args: Array[String]): Unit = {

    def dropTest[A](l: List[A], n: Int, expected: List[A]) = {
      println(
        s"For list $l dropping $n leaves ${List.drop(l, n)} and expected ${expected}"
      )
    }

    dropTest(List("1", "2", "3"), 1, List("2", "3"))
    dropTest(List("1", "2", "3"), 2, List("3"))
    dropTest(List("1", "2", "3"), 3, Nil)
    dropTest(List(1, 2, 3, 4), 3, List(4))
  }
}
