import scala.annotation.tailrec

object Ch05 {


  // QUESTION: If laziness is such a great thing why are functions not by default as lazy as possible,
  // i.e. I need to explicitly write (=> A) to make functions diligent
  sealed trait Stream[+A] {

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    def myString: String = {
      def go[A](sa: Stream[A]): String = sa match {
        case Empty => ""
        case Cons(h, t) => if (t() == Empty) h().toString else h().toString + "," + go[A](t())
      }
      "(" + go(this) + ")"
    }

    // QUESTION: Why do we want to force the evaluation ? Laziness is cool, isn't it ?
    // 5.1 Implement toList function that convert a Stream to a List, which will force its evaluation and let you
    // look at it in the REPL. You can convert to the regular List type in the standard library.
    def toList: List[A] = {
      @tailrec
      def go(str: Stream[A])(as: List[A]): List[A] = str match {
        case Empty => as
        case Cons(h, t) => go(t())(List.Cons[A](h(), as))
      }
      List.reverse(go(this)(List.Nil))
    }

    // 5.2 Write the function take(n) for returning the first n elements of a Stream,
    // and drop(n) for skipping the first n elements of a Stream.
    final def take(n: Int): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) => if (n > 0) Cons(h, () => t().take(n - 1)) else Empty
    }

    @tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) => if (n > 0) t().drop(n - 1) else Cons(h, t)
    }

    // 5.3 Write the function takeWhile for returning all starting elements of
    // a Stream that match the given predicate.
    final def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) => if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
    }

    // 5.4 Implement forAll, which checks that all elements in the Stream match a given predicate.
    // Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
    final def forAll(p: A => Boolean): Boolean = !this.exists(a => !p(a))

    // One could implement exists using foldRight. However foldRight is not tail recursive :(
    @tailrec
    final def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    final def foldRight[B](z: => B)(f: (=> A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    // 5.5 Use foldRight to implement takeWhile.
    final def takeWhileFoldRight(p: (=> A) => Boolean): Stream[A] = foldRight[Stream[A]](Empty)((a, s) => if (p(a)) Cons(() => a, () => s) else Empty)

    // 5.6 Hard: Implement headOption using foldRight.
    final def headOptionFoldRight: Option[A] = foldRight[Option[A]](None)((a, _) => Some(a))

    // 5.7 Implement map, filter, append, and flatMap using foldRight.
    // The append method should be non-strict in its argument.
    final def append[B >: A](s2: Stream[B]): Stream[B] = foldRight[Stream[B]](s2)((a, s) => Cons[B](() => a, () => s))

    def map[B](f: (=> A) => B): Stream[B] = foldRight[Stream[B]](Empty)((a, bs) => Cons(() => f(a), () => bs))

    def filter(p: (=> A) => Boolean): Stream[A] = foldRight[Stream[A]](Empty)((a, s) => if (p(a)) Cons(() => a, () => s) else s)

    def flatMap[B](f: (=> A) => Stream[B]): Stream[B] = foldRight[Stream[B]](Empty)((a, s) => f(a).append(s))


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

    // apply function to generate infinite streams
    final def apply[A](f: (=> Int) => A): Stream[A] = {
      def go[A](f: (=> Int) => A)(n: Int): Stream[A] = Cons(() => f(n), () => go(f)(n + 1))
      //      def go[A](f: (=>Int) => A)(n: Int): Stream[A] = cons(f(n), go(f)(n + 1))
      go(f)(0)
    }
  }


  // 5.8 Generalize ones slightly to the function constant, which
  // returns an infinite Stream of a given value.
  def constant[A](a: A): Stream[A] = ???


  // 5.9 Write a function that generates an infinite stream of integers,
  // starting from n, then n + 1, n + 2, and so on.7
  def from(n: Int): Stream[Int] = ???


  // 5.10 Write a function fibs that generates the infinite stream of
  // Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.


  // 5.11 Write a more general stream-building function called unfold.
  // It takes an initial state, and a function for producing both the next state
  // and the next value in the generated stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???


  // 5.12 Write fibs, from, constant, and ones in terms of unfold.


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

}

object nith_Chapter_05 extends App {

  val lazyIdentiy: (=> Int) => Int = n => n
  val lazySquare: (=> Int) => Int = n => n * n
  val lazyMultiple: (=> Int) => (=> Int) => Int = n => i => n * i
  // streams
  val emptyStream: Ch05.Stream[Int] = Ch05.Stream()
  val oneStream: Ch05.Stream[Int] = Ch05.Stream(0)
  val fiveStream: Ch05.Stream[Int] = Ch05.Stream(0, 1, 2, 3, 4)
  val identityStream: Ch05.Stream[Int] = Ch05.Stream[Int](lazyIdentiy)
  val squareStream: Ch05.Stream[Int] = Ch05.Stream[Int](lazySquare)
  val streamOfMultiples: (=> Int) => Ch05.Stream[Int] = n => Ch05.Stream[Int](lazyMultiple(n))
  val stringStream: Ch05.Stream[String]
  = Ch05.Stream[String]("Hello", "This is my first stream of strings.", "I hope you like it.", "Enjoy !")

  println("****** Chapter_05 ******")

  println("Empty = " + Ch05.Empty.myString)
  println("oneStream = " + oneStream.myString)
  println("fiveStream = " + fiveStream.myString)
  println("stringStream = " + stringStream.myString)

  println("** Exercise 5.1 **")
  println("Empty.toList = " + Ch05.Empty.toList)
  println("fiveStream.toList = " + fiveStream.toList)
  println("** Exercise 5.2 **")
  println("** take ")
  println("Empty.take(1) = " + Ch05.Empty.take(1).myString)
  println("fiveStream.take(-1) = " + fiveStream.take(-1).myString)
  println("fiveStream.take(0) = " + fiveStream.take(0).myString)
  println("fiveStream.take(3) = " + fiveStream.take(3).myString)
  println("fiveStream.take(7) = " + fiveStream.take(7).myString)
  println("identityStream.take(42) = " + identityStream.take(42).myString)
  println("squareStream.take(42) = " + squareStream.take(42).myString)
  println("** drop ")
  println("Empty.drop(1) = " + Ch05.Empty.drop(1).myString)
  println("fiveStream.drop(-1) = " + fiveStream.drop(-1).myString)
  println("fiveStream.drop(0) = " + fiveStream.drop(0).myString)
  println("fiveStream.drop(3) = " + fiveStream.drop(3).myString)
  println("fiveStream.drop(7) = " + fiveStream.drop(7).myString)
  println("identityStream.drop(10).take(42) = " + identityStream.drop(10).take(42).myString)
  println("squareStream.drop(10).take(42) = " + squareStream.drop(10).take(42).myString)
  println("** Exercise 5.3 **")
  println("Empty.takeWhile(1) = " + Ch05.Empty.takeWhile(_ => true).myString)
  println("fiveStream.takeWhile(_%2==0) = " + fiveStream.takeWhile(_ % 2 == 0).myString)
  println("identityStream.takeWhile( n => (n%2==0) || (n<10) ) = "
    + identityStream.takeWhile(n => (n % 2 == 0) || (n < 10)).myString)
  println("squareStream.takeWhile( n => (n%2==0) || (n<10) ) = "
    + squareStream.takeWhile(n => (n % 2 == 0) || (n < 10)).myString)
  println("** Exercise 5.4 **")
  println("fiveStream.forAll(_<6) = " + fiveStream.forAll(_ < 6))
  println("fiveStream.forAll(_%2==0) = " + fiveStream.forAll(_ % 2 == 0))
  println("identityStream.forAll(_<1000) = " + identityStream.forAll(_ < 1000))
  println("squareStream.forAll(_<10000) = " + squareStream.forAll(_ < 10000))
  println("** Exercise 5.5 **")
  println("Empty.takeWhileFoldRight(1) = " + Ch05.Empty.takeWhileFoldRight(_ => true).myString)
  println("fiveStream.takeWhileFoldRight(_%2==0) = " + fiveStream.takeWhileFoldRight(_ % 2 == 0).myString)
  println("identityStream.takeWhileFoldRight( n => (n%2==0) || (n<10) ) = "
    + identityStream.takeWhileFoldRight(n => (n % 2 == 0) || (n < 10)).myString)
  println("squareStream.takeWhileFoldRight( n => (n%2==0) || (n<10) ) = "
    + squareStream.takeWhileFoldRight(n => (n % 2 == 0) || (n < 10)).myString)
  println("** Exercise 5.6 **")
  println("Empty.headOptionFoldRight = " + Ch05.Empty.headOptionFoldRight)
  println("fiveStream.headOptionFoldRight = " + fiveStream.headOptionFoldRight)
  println("identityStream.headOptionFoldRight = " + identityStream.headOptionFoldRight)
  println("squareStream.headOptionFoldRight = " + squareStream.headOptionFoldRight)
  println("identityStream.drop(42).headOptionFoldRight = " + identityStream.drop(42).headOptionFoldRight)
  println("** Exercise 5.7 **")
  println("** append ")
  println("emptyStream.append(fiveStream) = " + emptyStream.append(fiveStream).myString)
  println("oneStream.append(fiveStream) = " + oneStream.append(fiveStream).myString)
  println("fiveStream.append(fiveStream) = " + fiveStream.append(fiveStream).myString)
  println("fiveStream.append(squareStream).take(20) = " + fiveStream.append(squareStream).take(20).myString)
  println("squareStream.append(identityStream).take(10) = " + squareStream.append(identityStream).take(10).myString)
  println("identityStream.append(squareStream).take(10) = " + identityStream.append(squareStream).take(10).myString)
  println("identityStream.append(identityStream).append(identityStream).append(identityStream).take(20) = "
    + identityStream.append(identityStream).append(identityStream).append(identityStream).take(20).myString)
  println("** map ")
  println("stringStream.map(_.length) = " + stringStream.map(_.length).myString)
  println("** filter ")
  println("identityStream.filter( n => (n%2==0) || (n<10) ).take(42) = "
    + identityStream.filter(n => (n % 2 == 0) || (n < 10)).take(42).myString)
  println("squareStream.filter( n => (n%2==0) || (n<10) ).take(42) = "
    + squareStream.filter(n => (n % 2 == 0) || (n < 10)).take(42).myString)
  println("** flatMap ")
  println("streamOfMultiples(5).take(20)=" + streamOfMultiples(5).take(20).myString)
  println("fiveStream.flatMap(n=>Ch05.Stream(n-1,n,n+1)) = "
    + fiveStream.flatMap(n => Ch05.Stream(n - 1, n, n + 1)).myString)
  println("fiveStream.drop(3).flatMap(streamOfMultiples).take(20) = "
    + fiveStream.drop(3).flatMap(streamOfMultiples).take(20).myString)

  println("!!!!! NOT FINISHED !!!!!")
  // Question: Why does this not terminate. What a pity !
  println("!!!!! squareStream.flatMap[Int](n=>Ch05.Stream[Int](n)).take(20) = Exception in thread \"main\" java.lang.StackOverflowError")
  // println("squareStream.flatMap[Int](n=>Ch05.Stream[Int](n)).take(20) = " + squareStream.flatMap[Int](n=>Ch05.Stream[Int](n)).take(20))

  // Question: Why does this not terminate. What a pity !
  println("!!!!! squareStream.flatMap(streamOfMultiples).take(42) = Exception in thread \"main\" java.lang.StackOverflowError")
  // println("squareStream.flatMap(streamOfMultiples).take(42) = " + squareStream.flatMap(streamOfMultiples).take(42).myString)

  // Question: Why does this not terminate. What a pity !
  println("!!!!! squareStream.drop(3).flatMap(streamOfMultiples).take(20) = Exception in thread \"main\" java.lang.StackOverflowError")
  // println("squareStream.drop(3).flatMap(streamOfMultiples).take(20) = " + squareStream.drop(3).flatMap(streamOfMultiples).take(20))
  println("!!!!! NOT FINISHED !!!!!")
}
