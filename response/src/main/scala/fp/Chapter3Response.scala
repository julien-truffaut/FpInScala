package fp

object Chapter3Response {

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


  }






}
