package fp


object Chapter3 {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]


  object List {

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil          => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs)  => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    // 3.1 what is the value of x
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y case Cons(h, t) => h + sum(t)
      case _ => 101
    }


    // 3.2 What are different choices you could make in your implementation if the List is Nil?
    def tail[A](xs: List[A]) = ???

    // 3.3
    def setHead[A](head: A, xs: List[A]): List[A] = ???

    // 3.4
    def drop[A](l: List[A], n: Int): List[A] = ???

    // 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

    // 3.6
    def init[A](l: List[A]): List[A] = ???

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)
    def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

    // 3.7 can we implement using foldRight? Can we stop iteration if we encounter 0.0

    // 3.8 what is the result of... What is the relationship between foldRight and Cons.apply
    foldRight(List(1,2,3), Nil: List[Int])(Cons(_, _))

    // 3.9 length in term of foldRight
    def length[A](as: List[A]): Int = ???

    // 3.10 verify that foldRight is not tailrec, implement tailrec foldLeft
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = ???

    // 3.11 write sum, product, and a function to compute the length of a list using foldLeft.

    // 3.12 write reverse using fold

    // 3.13 write foldLeft in terms of foldRight, can we do in the other way around?

    // 3.14 Implement append in terms of either foldLeft or foldRight.

    // 3.15 Hard: Write a function that concatenates a list of lists into a single list.
    // Its runtime should be linear in the total length of all lists. Try to use functions we have already defined.


  }






}
