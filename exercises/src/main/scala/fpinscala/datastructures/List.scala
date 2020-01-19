package fpinscala.datastructures

import scala.annotation.tailrec

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

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _                  => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil           => sys.error("init of empty list")
    case Cons(_, Nil)  => Nil
    case Cons(x, tail) => Cons(x, init(tail))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil           => z
    case Cons(x, tail) => foldLeft(tail, f(z, x))(f)
  }

  def sum3(l: List[Int]): Int = {
    foldLeft(l, 0)(_ + _)
  }

  def product3(l: List[Double]): Double = {
    foldLeft(l, 0.0)(_ * _)
  }

  def length2[A](l: List[A]): Int = {
    foldLeft(l, 0)((acc, _) => acc + 1)
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((acc, x) => Cons(x, acc))

  def reverse2[A](l: List[A]): List[A] = {
    @tailrec
    def go(list: List[A], acc: List[A]): List[A] = list match {
      case Nil           => acc
      case Cons(x, tail) => go(tail, Cons(x, acc))
    }
    go(l, Nil)
  }

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((x, y) => f(y, x))

  def foldRight3[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, Predef.identity[B] _)((acc, a) => (x: B) => acc(f(a, x)))(z)

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, Predef.identity[B] _)((a, acc) => (x: B) => acc(f(x, a)))(z)

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons.apply)

  def flatten[A](l: List[List[A]]): List[A] = l match {
    case Nil                      => Nil
    case Cons(Nil, Cons(a2, Nil)) => a2
    case Cons(Cons(head, tail), Cons(a2, Nil)) =>
      Cons(head, flatten(List[List[A]](tail, a2)))
    case Cons(x, tail) => flatten(List[List[A]](x, flatten(tail)))
  }

  def flatten2[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  def mkString[A](l: List[A], sep: String): String =
    foldLeft(l, "(")(_ + _ + sep).dropRight(sep.length) + ")"

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((a, acc) => Cons(a + 1, acc))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((a, acc) => Cons(a.toString, acc))

  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil           => Nil
    case Cons(x, tail) => Cons(f(x), map(tail)(f))
  }

  def map2[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((x, acc) => Cons(f(x), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((a, acc) => if (f(a)) Cons(a, acc) else acc)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil: List[B])((x, acc) => append(f(x), acc))

  def filterViaflatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) List(x) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, tailX), Cons(y, tailY)) =>
      Cons(x + y, addPairwise(tailX, tailY))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, tailX), Cons(y, tailY)) =>
        Cons(f(x, y), zipWith(tailX, tailY)(f))
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {

    def go[A](a: List[A], b: List[A], failBack: => List[A]): Boolean =
      (a, b) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(x, tailA), Cons(y, tailB)) =>
          if (x == y) go(tailA, tailB, failBack)
          else go(failBack, sub, List.tail(failBack))
      }

    go(sup, sub, List.tail(sup))
  }

  @tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(a, tailA), Cons(b, tailB)) if (a == b) =>
      startsWith(tailA, tailB)
    case _ => false
  }

  @tailrec
  def hasSubsequence2[A](sup: List[A], sub: List[A]): Boolean =
    sup match {
      case Nil                            => sub == Nil
      case _ if List.startsWith(sup, sub) => true
      case Cons(_, tail)                  => hasSubsequence2(tail, sub)
    }
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

object Exercise35 {

  def main(args: Array[String]): Unit = {

    def dropWhileTest[A](l: List[A], f: A => Boolean, expected: List[A]) = {
      println(
        s"For list $l dropping while leaves ${List.dropWhile(l, f)} and expected ${expected}"
      )
    }

    dropWhileTest(List(1, 2, 3, 4), (x: Int) => x < 4, List(4))
    dropWhileTest(List(1, 2, 3, 4), (x: Int) => x < 2, List(2, 3, 4))
    dropWhileTest(List(1, 2, 3, 4), (x: Int) => x < 5, Nil)
  }
}

object Exercise39 {

  def main(args: Array[String]): Unit = {

    def lengthTest[A](l: List[A], expected: Int) = {
      println(
        s"For list $l the actual length is ${List.length(l)} and expected ${expected}"
      )
    }

    lengthTest(List(1, 2, 3, 4), 4)
    lengthTest(List(1, 2, 3), 3)
    lengthTest(Nil, 0)
  }
}

object Exercise310 {

  def main(args: Array[String]): Unit = {

    def foldLeftTest[A, B](l: List[A], z: B, f: (B, A) => B, expected: B) = {
      println(
        s"For list $l folding left result is ${List.foldLeft(l, z)(f)} and expected ${expected}"
      )
    }

    foldLeftTest(List(1, 2, 3, 4), 0, (acc: Int, x: Int) => acc + x, 10)
    foldLeftTest(Nil, 10, (acc: Int, x: Int) => acc + x, 10)
    foldLeftTest(
      List("1", "2", "3"),
      "",
      (acc: String, x: String) => acc + x,
      "123"
    )
    foldLeftTest(
      List(1, 2, 3, 4),
      "",
      (acc: String, x: Int) => acc + x.toString,
      "1234"
    )

  }
}

object Exercise311 {

  def main(args: Array[String]): Unit = {

    def reverseTest[A](l: List[A], expected: List[A]) = {
      println(
        s"For list $l reverse result is ${List.reverse(l)} and expected ${expected}"
      )
    }

    reverseTest(List(1, 2, 3, 4), List(4, 3, 2, 1))
    reverseTest(List(1), List(1))
    reverseTest(Nil, Nil)
  }
}

object Exercise312 {

  def main(args: Array[String]): Unit = {

    def foldLeftTest2[A, B](l: List[A], z: B, f: (B, A) => B, expected: B) = {
      println(
        s"For list $l folding left result is ${List.foldLeft2(l, z)(f)} and expected ${expected}"
      )
    }

    def foldRightTest3[A, B](l: List[A], z: B, f: (A, B) => B, expected: B) = {
      println(
        s"For list $l folding right result is ${List.foldRight3(l, z)(f)} and expected ${expected}"
      )
    }

    foldLeftTest2(List(1, 2, 3, 4), 0, (acc: Int, x: Int) => acc + x, 10)
    foldLeftTest2(Nil, 10, (acc: Int, x: Int) => acc + x, 10)
    foldLeftTest2(
      List("1", "2", "3"),
      "",
      (acc: String, x: String) => acc + x,
      "123"
    )
    foldLeftTest2(
      List(1, 2, 3, 4),
      "",
      (acc: String, x: Int) => acc + x.toString,
      "1234"
    )

    foldRightTest3(List(1, 2, 3, 4), 0, (x: Int, acc: Int) => acc + x, 10)
    foldRightTest3(Nil, 10, (x: Int, acc: Int) => acc + x, 10)
    foldRightTest3(
      List("1", "2", "3"),
      "",
      (x: String, acc: String) => acc + x,
      "321"
    )
    foldRightTest3(
      List(1, 2, 3, 4),
      "",
      (x: Int, acc: String) => acc + x.toString,
      "4321"
    )

  }
}

object Exercise314 {

  def main(args: Array[String]): Unit = {

    def appendTest[A](l: List[A], l2: List[A], expected: List[A]) = {
      println(
        s"For list $l and $l2 append result is ${List.append2(l, l2)} and expected ${expected}"
      )
    }

    appendTest(List(1, 2, 3, 4), List(5, 6, 7, 8), List(1, 2, 3, 4, 5, 6, 7, 8))
    appendTest(List(1, 2, 3, 4), Nil, List(1, 2, 3, 4))
    appendTest(Nil, List(1, 2, 3, 4), List(1, 2, 3, 4))
    appendTest(Nil, Nil, Nil)
  }
}

object Exercise315 {

  def main(args: Array[String]): Unit = {

    def testFlatten[A](l: List[List[A]], expected: List[A]) = {
      val inputString = List.mkString(List.map(l)(List.mkString(_, ", ")), ", ")
      println(
        s"For list $inputString flatten result is ${List
          .mkString(List.flatten2(l), ", ")} " +
          s"and expected ${List.mkString(expected, ", ")}"
      )
    }

    testFlatten(
      List(List(1, 2), List(3, 4), List(5), Nil, List(6), Nil),
      List(1, 2, 3, 4, 5, 6)
    )

  }
}

object Exercise319 {

  def main(args: Array[String]): Unit = {

    def testFilter[A](l: List[A], f: A => Boolean, expected: List[A]) = {
      val inputString = List.mkString(l, ", ")
      println(
        s"For list $inputString filter result is ${List
          .mkString(List.filter(l)(f), ", ")} " +
          s"and expected ${List.mkString(expected, ", ")}"
      )
    }

    testFilter(List(1, 2, 3, 4, 5, 6), (x: Int) => x % 2 == 0, List(2, 4, 6))
    testFilter(
      List(1, 2, 3, 4, 5, 6),
      (x: Int) => x < 20,
      List(1, 2, 3, 4, 5, 6)
    )
    testFilter(List(1, 2, 3, 4, 5, 6), (x: Int) => x > 20, Nil)
    testFilter(Nil, (x: Int) => true, Nil)
  }
}

object Exercise322 {

  def main(args: Array[String]): Unit = {

    def testAddPairwise(a: List[Int], b: List[Int], expected: List[Int]) = {
      val inputString1 = List.mkString(a, ", ")
      val inputString2 = List.mkString(b, ", ")
      println(
        s"For list $inputString1 and $inputString2 addPairwise result is ${List
          .mkString(List.addPairwise(a, b), ", ")} " +
          s"and expected ${List.mkString(expected, ", ")}"
      )
    }

    testAddPairwise(
      List(1, 2, 3, 4, 5, 6),
      List(1, 2, 3, 4, 5, 6),
      List(2, 4, 6, 8, 10, 12)
    )
    testAddPairwise(Nil, Nil, Nil)
    testAddPairwise(
      List(1, 2, 3, 4, 5, 6),
      List(1, 2, 3, 4, 5),
      List(2, 4, 6, 8, 10)
    )
  }
}

object Exercise323 {

  def main(args: Array[String]): Unit = {

    def testZipWith[A, B, C](a: List[A],
                             b: List[B])(f: (A, B) => C)(expected: List[C]) = {
      val inputString1 = List.mkString(a, ", ")
      val inputString2 = List.mkString(b, ", ")
      println(
        s"For list $inputString1 and $inputString2 zipWith result is ${List
          .mkString(List.zipWith(a, b)(f), ", ")} " +
          s"and expected ${List.mkString(expected, ", ")}"
      )
    }

    testZipWith(List(1, 2, 3, 4, 5, 6), List(1, 2, 3, 4, 5, 6))(
      (a: Int, b: Int) => a + b
    )(List(2, 4, 6, 8, 10, 12))
    testZipWith(Nil, Nil)((a: Int, b: Int) => a + b)(Nil)
    testZipWith(List(1, 2, 3, 4), List("a", "b", "c"))(
      (a: Int, b: String) => a.toString + b
    )(List("1a", "2b", "3c"))
  }
}

object Exercise324 {

  def main(args: Array[String]): Unit = {

    def testHasSubsequence[A](a: List[A], b: List[A], expected: Boolean) = {
      val inputString1 = List.mkString(a, ", ")
      val inputString2 = List.mkString(b, ", ")
      println(
        s"For list $inputString1 and $inputString2 hasSubsequence result is ${List
          .hasSubsequence2(a, b)} " +
          s"and expected $expected"
      )
    }

    testHasSubsequence(List(1, 2, 3, 4, 5, 6), List(1, 2, 3, 4, 5, 6), true)
    testHasSubsequence(List(1, 2, 3, 4, 5, 6), List(1, 2, 3), true)
    testHasSubsequence(List(1, 2, 3, 4, 5, 6), List(4, 5, 6), true)
    testHasSubsequence(List(1, 2, 3, 4, 5, 6), List(4, 5), true)
    testHasSubsequence(List(1, 2, 3, 4, 5, 6), List(6), true)
    testHasSubsequence(List(1, 2, 3, 4, 5, 6), List(1), true)
    testHasSubsequence(List(1, 2, 3, 4, 5, 6), Nil, true)
    testHasSubsequence(List(1, 2, 3, 4, 5, 6), List(2, 4), false)
    testHasSubsequence(List(1, 2, 3, 4, 5, 6), List(5), true)
    testHasSubsequence(Nil, List(4, 5, 6), false)
  }
}
