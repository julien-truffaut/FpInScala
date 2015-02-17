package gerard

object Chapter5 {

  import Stream._

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty      => None
      case Cons(h, t) => Some(h())
    }

    // 5.1 Implement toList function that convert a Stream to a List, which will force its evaluation and let you
    // look at it in the REPL. You can convert to the regular List type in the standard library.

    def toList: List[A] = this match {
      case Empty      => Nil
      case Cons(h, t) => h() :: t().toList
    }

    // 5.2 Write the function take(n) for returning the first n elements of a Stream,
    // and drop(n) for skipping the first n elements of a Stream.
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _                   => Empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case s                   => s
    }

    // 5.3 Write the function takeWhile for returning all starting elements of
    // a Stream that match the given predicate.
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _                    => empty
    }

    // 5.4 Implement forAll, which checks that all elements in the Stream match a given predicate.
    // Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
    def forAll(p: A => Boolean): Boolean = {
      foldRight(true) {
        (h, t) => p(h) && t
      }
    }

    // 5.5 Use foldRight to implement takeWhile.
    def takeWhile2(p: A => Boolean): Stream[A] = {
      foldRight(empty[A]) {
        (h, t) =>
          if (p(h))
            cons(h, t)
          else
            empty
      }
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _          => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

    // 5.6 Hard: Implement headOption using foldRight.
    def headOption2: Option[A] = {
      foldRight(None: Option[A]) {
        (h, _) => Some(h)
      }
    }

    // 5.7 Implement map, filter, append, and flatMap using foldRight.
    // The append method should be non-strict in its argument.
    def map[B](f: A => B): Stream[B] = {
      // for some reason this method reads one value too much... ???
      foldRight(empty: Stream[B]) {
        (h, t) => cons(f(h), t)
      }
    }

    def flatMap[B](f: A => Stream[B]): Stream[B] = {
      foldRight(empty: Stream[B]) {
        (h, t) => f(h).append(t)
      }
    }

    def filter(f: A => Boolean): Stream[A] = {
      foldRight(empty: Stream[A]) {
        (h, t) => if (f(h)) cons[A](h, t) else t
      }
    }

    def append[B >: A](stream: Stream[B]): Stream[B] = {
      foldRight(stream) {
        (h, t) => cons(h, t)
      }
    }

    def map0[B](f: A => B): Stream[B] = {
      unfold(this) {
        case Cons(h, t) => Some(f(h()) -> t())
        case Empty      => None
      }
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = ???

    def tails: Stream[Stream[A]] = ???

    def startsWith[B](s: Stream[B]): Boolean = ???

    def hasSubsequence[B](s: Stream[B]): Boolean =
      tails exists (_ startsWith s)

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }


  // 5.8 Generalize ones slightly to the function constant, which
  // returns an infinite Stream of a given value.
  def constant[A](a: A): Stream[A] = cons(a, constant(a))


  // 5.9 Write a function that generates an infinite stream of integers,
  // starting from n, then n + 1, n + 2, and so on.7
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))


  // 5.10 Write a function fibs that generates the infinite stream of
  // Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
  def fibs(a: Int = 0, b: Int = 1): Stream[Int] = {
    cons(a, fibs(a + b, a))
  }

  // 5.11 Write a more general stream-building function called unfold.
  // It takes an initial state, and a function for producing both the next state
  // and the next value in the generated stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None         => Empty
    }
  }


  // 5.12 Write fibs, from, constant, and ones in terms of unfold.
  def constant0[A](a: A): Stream[A] = unfold(a)(_ => Some(a -> a))

  def from0(n: Int): Stream[Int] = unfold(n)(n => Some((n + 1, n + 1)))

  def fibs0: Stream[Int] = unfold(0 -> 1)(s => Some((s._1, s._1 + s._2 -> s._1)))


  // 5.13 Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3),
  // and zipAll. The zipAll function should continue the traversal as long as either
  // stream has more elements—it uses Option to indicate whether each stream has been exhausted.


  // 5.14 Hard: Implement startsWith using functions you’ve written. It should check
  // if one Stream is a prefix of another. For instance, Stream(1,2,3)
  // startsWith Stream(1,2) would be true.


  // 5.15 Implement tails using unfold. For a given Stream, tails returns the Stream of
  // suffixes of the input sequence, starting with the original Stream. For example,
  // given Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).


  // 5.16 Hard: Generalize tails to the function scanRight, which is like a foldRight that returns
  // a stream of the intermediate results. For example:
  // scala> Stream(1,2,3).scanRight(0)(_ + _).toList
  // res0: List[Int] = List(6,5,3,0)
  // This example should be equivalent to the expression List(1+2+3+0, 2+3+0, 3+0, 0).
  // Your function should reuse intermediate results so that traversing a Stream with n elements
  // always takes time linear in n. Can it be implemented using unfold? How, or why not? Could it
  // be implemented using another function we’ve written?

  def main(args: Array[String]) {
    import Stream._
    val a = Cons(() => {
      println("1")
      1
    }, () => Cons(() => {
      println("2")
      2
    }, () => Cons(() => {
      println("3")
      3
    }, () => Cons(() => {
      println("4")
      4
    }, () => empty))))

    println(a.take(2).toList)
    println(a.drop(1).toList)
    println(a.takeWhile(_ < 3).toList)
    println(a.takeWhile2(_ < 3).toList)
    println(a.forAll(_ < 1))
    println(a.headOption)
    println(a.headOption2)
    println(empty.headOption2)
    println("map: " + a.map(_ * 5).take(2).toList)
    println("filter: " + a.filter(_ % 2 == 0).toList)
    println("append: " + cons("a", cons("b", empty)).append(a).toList)
    println("flatMap: " + a.flatMap(i => cons(i, cons(i, empty))).toList)
    println("constant: " + constant(42).take(3).toList)
    println("from: " + from(42).take(3).toList)
    println("fibs: " + fibs().take(7).toList)
    println("constant: " + constant0(42).take(3).toList)
    println("from: " + from0(42).take(3).toList)
    println("fibs: " + fibs0.take(8).toList)
    println("map: " + a.map0(_ * 5).take(2).toList)
  }
}
