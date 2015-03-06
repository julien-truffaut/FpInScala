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
      def go[A](sa: => Stream[A]): String = sa match {
        case Empty => ""
        case Cons(h, t) => {
          lazy val hString: String = h() match {
            case x@Empty => "()"
            case x@Cons(_, _) => x.myString
            case x => x.toString
          }
          if (t() == Empty) hString else hString + "," + go[A](t())
        }
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

    def toList2: List[A] = this match {
      case Empty => List.Nil
      case Cons(h, t) => List.Cons[A](h(), t().toList2)
    }

    // 5.2 Write the function take(n) for returning the first n elements of a Stream,
    // and drop(n) for skipping the first n elements of a Stream.
    final def take(n: Int): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) if (n < 1) => {
        //        println("...take("+n+"): nothing to do. Supi !")
        Empty
      }
      case Cons(h, t) if (n > 0) => {
        //        println("...take("+n+"): Shit! This guy wants sth from me !")
        Cons(h, () => t().take(n - 1))
      }
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

    final def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      // println("...foldRight(%s)".format("this.getClass()="+this.getClass().toString+"  this.take(10)="+this.take(10).myString + " , z=" + z))
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    }

    @tailrec
    final def foldLeft[B](z: B)(f: A => B => B): B = {
      // println("...foldLeft(%s)".format("this.getClass()="+this.getClass().toString+"  this.take(10)="+this.take(10).myString + " , z=" + z))
      this match {
        case Cons(a, as) => as().foldLeft(f(a())(z))(f)
        case _ => z
      }
    }

    def reverse: Stream[A] = this.foldLeft(Empty: Stream[A])(a => aStream => Cons(() => a, () => aStream))


    // 5.5 Use foldRight to implement takeWhile.
    final def takeWhileFoldRight(p: (=> A) => Boolean): Stream[A] = foldRight[Stream[A]](Empty)((a, s) => if (p(a)) Cons(() => a, () => s) else Empty)

    // 5.6 Hard: Implement headOption using foldRight.
    final def headOptionFoldRight: Option[A] = foldRight[Option[A]](None)((a, _) => Some(a))

    // 5.7 Implement map, filter, append, and flatMap using foldRight.
    // The append method should be non-strict in its argument.
    final def append[B >: A](s2: => Stream[B]): Stream[B] = foldRight[Stream[B]](s2)((a, s) => Stream.cons[B](a, s))

    final def streamAppend[B >: A](ss: Stream[Stream[B]]): Stream[B] = this.append(ss.foldRight[Stream[B]](Empty)((a, s) => a.append(s)))

    final def map[B](f: (=> A) => B): Stream[B] = foldRight[Stream[B]](Empty)((a, bs) => Cons(() => f(a), () => bs))

    final def filter(p: (=> A) => Boolean): Stream[A] = foldRight[Stream[A]](Empty)((a, s) => if (p(a)) Cons(() => a, () => s) else s)

    final def flatMap[B](f: (=> A) => Stream[B]): Stream[B] = foldRight[Stream[B]](Empty)((a, s) => f(a).append(s))

    // 5.13 Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3),
    // and zipAll. The zipAll function should continue the traversal as long as either
    // stream has more elements—it uses Option to indicate whether each stream has been exhausted.
    final def mapUnfold[B](f: A => Option[B]): Stream[B]
    = unfold[B, Stream[A]](this)(aStream => aStream match {
      case Cons(a, aTail) => f(a()) match {
        case Some(b) => Some(b, aTail())
        case _ => None
      }
      case _ => None
    })

    final def takeUnfold(n: Int): Stream[A] = unfold[A, (Stream[A], Int)]((this, 0))(x => x._1 match {
      case Cons(a, as) if x._2 < n => Some(a(), (as(), 1 + x._2))
      case _ => None
    })

    final def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold[A, Stream[A]](this)(aStream => aStream match {
      case Cons(a, aTail) if p(a()) => Some(a(), aTail())
      case _ => None
    })

    final def zipWith[B, C](bs: Stream[B])(f: A => B => C): Stream[C]
    = unfold[C, (Stream[A], Stream[B])]((this, bs))(x => x._1 match {
      case Cons(a, aTail) => x._2 match {
        case Cons(b, bTail) => Some(f(a())(b()), (aTail(), bTail()))
        case _ => None
      }
      case _ => None
    })

    // there is some potential for improvement
    final def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])]
    = unfold[(Option[A], Option[B]), (Stream[A], Stream[B])]((this, s2))(x => x._1 match {
      case Cons(a, aTail) => x._2 match {
        case Cons(b, bTail) => Some((Some(a()), Some(b())), (aTail(), bTail()))
        case _ => Some((Some(a()), None), (aTail(), Empty))
      }
      case _ => x._2 match {
        case Cons(b, bTail) => Some((None, Some(b())), (Empty, bTail()))
        case _ => None
      }
    })

    // 5.14 Hard: Implement startsWith using functions you’ve written. It should check
    // if one Stream is a prefix of another. For instance, Stream(1,2,3)
    // startsWith Stream(1,2) would be true.
    final def startsWith[A](s: Stream[A]): Boolean = this.zipAll(s).forAll(x => (x._2 == None) || x._1 == x._2)


    // 5.15 Implement tails using unfold. For a given Stream, tails returns the Stream of
    // suffixes of the input sequence, starting with the original Stream. For example,
    // given Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).
    final def tails: Stream[Stream[A]] = unfold[Stream[A], Stream[A]](this)(aStream => aStream match {
      case Cons(a, aTail) => Some(aTail(), aTail())
      case Empty => None
    })

    final def hasSubsequence[B](s: Stream[B]): Boolean = tails.exists(_ startsWith s)

    // 5.16 Hard: Generalize tails to the function scanRight, which is like a foldRight that returns
    // a stream of the intermediate results. For example:
    // scala> Stream(1,2,3).scanRight(0)(_ + _).toList
    // res0: List[Int] = List(6,5,3,0)
    // This example should be equivalent to the expression List(1+2+3+0, 2+3+0, 3+0, 0).
    // Your function should reuse intermediate results so that traversing a Stream with n elements
    // always takes time linear in n. Can it be implemented using unfold? How, or why not? Could it
    // be implemented using another function we’ve written?

    // *** scanLeft ***
    // scanRight requires the total result to be the first element of the result stream. Therefore this cannot work on
    // an infinite stream. Good reason to implement scanLeft: the first element contains the defined neutral element for
    // f. Unfortunately this works only if the result type is a super type of A.

    final def scanLeft[B >: A, C](f: B => C => C)(z: (C, B)): Stream[C] = {
      lazy val initialState: (C, Stream[B]) = (z._1, Stream.cons[B](z._2, this))
      lazy val unFoldFun: ((C, Stream[B])) => Option[(C, (C, Stream[B]))] = {
        case (z, bStream) => {
          //          println("... scanLeft: z="+z+"  bStream.take(20)=" + bStream.take(20).myString)
          bStream match {
            case Cons(b1, bTail1) => bTail1() match {
              case Cons(b2, bTail2) =>
                val nextResult: C = f(b2())(z)
                Some[(C, (C, Stream[B]))](z, (nextResult, bTail1()))
              case _ => Some[(C, (C, Stream[B]))](z, (z, Empty))
            }
            case _ => None
          }
        }
      }
      unfold[C, Tuple2[C, Stream[B]]](initialState)(unFoldFun)
    }


    final def scanLeft2[B >: A, C](f: B => C => C)(z: (C, B)): Stream[C] = {
      lazy val initialState: (C, Stream[B]) = (z._1, this)
      lazy val unFoldFun: ((C, Stream[B])) => Option[(C, (C, Stream[B]))] = {
        case (z, bStream) => {
          //          println("... scanLeft2: z="+z+"  bStream.take(20)=" + bStream.take(20).myString)
          bStream match {
            case Cons(b1, bTail1) =>
              val nextResult: C = f(b1())(z)
              Some[(C, (C, Stream[B]))](nextResult, (nextResult, bTail1()))
            case _ => None
          }
        }
      }
      Stream.cons(z._1, unfold[C, Tuple2[C, Stream[B]]](initialState)(unFoldFun))
    }

    // attemp to replace the bStream match in scanLeft2 by foldRight
    // but I feel that this does not give much sense
    final def scanLeft3[B >: A, C](f: B => C => C)(z: (C, B)): Stream[C] = {
      lazy val initialState: (C, Stream[B]) = (z._1, this)
      lazy val unFoldFun: ((C, Stream[B])) => Option[(C, (C, Stream[B]))] = {
        case (z, bStream) => {
          //          println("... scanLeft3: z="+z+"  bStream.take(20)=" + bStream.take(20).myString)
          bStream.foldRight[Option[(C, (C, Stream[B]))]](None)((b, ccb) => Some[(C, (C, Stream[B]))]((f(b)(ccb.get._1): C), ccb.get._2))
        }
      }
      Stream.cons(z._1, unfold[C, Tuple2[C, Stream[B]]](initialState)(unFoldFun))
    }


    final def scanRightReverse[B >: A, C](f: B => C => C)(z: (C, B)): Stream[C] = this.scanLeft[B, C](f)(z).reverse

    final def scanRightUnfoldReverse[B >: A, C](f: B => C => C)(z: (C, B)): Stream[C] = {
      lazy val initialState: (C, Stream[B]) = (z._1, Stream.cons[B](z._2, this))
      lazy val unFoldFun: ((C, Stream[B])) => Option[(C, (C, Stream[B]))] = {
        case (z, bStream) => {
          //          println("... scanRightUnfoldLeft: z="+z+"  bStream.take(20)=" + bStream.take(20).myString)
          bStream match {
            case Cons(b1, bTail1) => bTail1() match {
              case Cons(b2, bTail2) =>
                val nextResult: C = f(b2())(z)
                Some[(C, (C, Stream[B]))](z, (nextResult, bTail1()))
              case _ => Some[(C, (C, Stream[B]))](z, (z, Empty))
            }
            case _ => None
          }
        }
      }
      unfoldReverse[C, Tuple2[C, Stream[B]]](Empty)(initialState)(unFoldFun)
    }


    // *** Additional Experiments ***

    // write ZIP function
    final def doubleFoldRight[C, B](cs: => Stream[C])(zThis: => B)(zCs: => B)(f: A => C => (=> B) => B): B = {
      this match {
        case Cons(h, t) => cs match {
          case Cons(c, cTail) => f(h())(c())(t().doubleFoldRight(cTail())(zThis)(zCs)(f))
          case _ => zCs
        }
        case _ => zThis
      }
    }

    final def streamCons[B >: A](ss: Stream[Stream[B]]): Stream[Stream[B]]
    = doubleFoldRight[Stream[B], Stream[Stream[B]]](ss)(Empty)(Empty)(h => bs => rec => Cons[Stream[B]](() => Cons(() => h, () => bs), () => rec))
  }


  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


  object Stream {
    final def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    final def empty[A]: Stream[A] = Empty

    final def non[A]: Stream[Option[A]] = cons[Option[A]](None, non[A])

    final def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    // apply function to generate infinite streams
    final def apply[A](f: (=> Int) => A): Stream[A] = {
      def go[A](f: (=> Int) => A)(n: Int): Stream[A] = cons(f(n), go(f)(n + 1))
      go(f)(0)
    }

    final def recApply[A](z:A)(f: (=> A) => A): Stream[A] = {
      def go(aStream:Stream[A])(f: (=> A) => A): Stream[A] = cons(f(aStream.headOption.get), aStream)
      go(cons(z, empty))(f)
    }

  }


  // 5.8 Generalize ones slightly to the function constant, which
  // returns an infinite Stream of a given value.
  final def constant[A](a: A): Stream[A] = Stream(_ => a)


  // 5.9 Write a function that generates an infinite stream of integers,
  // starting from n, then n + 1, n + 2, and so on.7
  final def from(n: Int): Stream[Int] = Stream(i => i + n)

  // 5.10 Write a function fibs that generates the infinite stream of
  // Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
  final def fibs: () => Stream[BigInt] = () => {
    def go(x: BigInt, y: BigInt): Stream[BigInt] = Stream.cons[BigInt](x, go(y, x + y))
    go(0, 1)
  }


  // 5.11 Write a more general stream-building function called unfold.
  // It takes an initial state, and a function for producing both the next state
  // and the next value in the generated stream.
  final def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some(x) => Stream.cons[A](x._1, unfold(x._2)(f))
    case None => Empty
  }

  // uses tail rec but does not terminate on infinite state streams
  @tailrec
  final def unfoldReverse[A, S](aS: Stream[A])(z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some(x) => unfoldReverse[A, S](Stream.cons[A](x._1, aS))(x._2)(f)
    case None => aS
  }

  // 5.12 Write fibs, from, constant, and ones in terms of unfold.
  // calculates the Fibonacci numbers up to n
  final def fibUnfold(n :BigInt) : Stream[BigInt]
  = unfoldReverse[BigInt, Tuple2[BigInt, BigInt]](Empty)((0, 1))(x => if (x._1 < n) Some((x._1, (x._2, x._1 + x._2))) else None)

}

object nith_Chapter_05 extends App {

  val lazyIdentiy: (=> Int) => Int = n => n
  val lazySquare: (=> Int) => Int = n => n * n
  val lazyMultiple: (=> Int) => (=> Int) => Int = n => i => n * i
  // finite streams
  val emptyStream: Ch05.Stream[Int] = Ch05.Stream()
  val oneStream: Ch05.Stream[Int] = Ch05.Stream(0)
  val fiveStream: Ch05.Stream[Int] = Ch05.Stream(0, 1, 2, 3, 4)
  val tenStream: Ch05.Stream[Int] = Ch05.Stream(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  val stringStream: Ch05.Stream[String]
  = Ch05.Stream[String]("Hello", "This is my first stream of strings.", "I hope you like it.", "Enjoy !")
  // infinite streams
  val ones: Ch05.Stream[Int] = Ch05.Stream.cons(1, ones)
  val twos: Ch05.Stream[Int] = Ch05.Stream.cons(2, twos)
  val onesAndTwos: Ch05.Stream[Int] = ones.append(twos)
  val identityStream: Ch05.Stream[Int] = Ch05.Stream[Int](lazyIdentiy)
  val squareStream: Ch05.Stream[Int] = Ch05.Stream[Int](lazySquare)
  val streamOfMultiples: (=> Int) => Ch05.Stream[Int] = n => Ch05.Stream[Int](lazyMultiple(n))
  // stream of streams
  val streamOfFiniteStreams: Ch05.Stream[Ch05.Stream[Int]] = Ch05.Stream(n => Ch05.Stream(n - 1, n, n + 1))
  val finiteStreamOfInfiniteStreams: Ch05.Stream[Ch05.Stream[Int]] = Ch05.Stream(ones)
  val streamOfInfiniteStreams: Ch05.Stream[Ch05.Stream[Int]] = Ch05.Stream((n => streamOfMultiples(n)))

  lazy val onesList: List[Int] = List.Cons(1, onesList)

  println("****** Chapter_05 ******")
  println("Empty = " + Ch05.Empty.myString)
  println("oneStream = " + oneStream.myString)
  println("fiveStream = " + fiveStream.myString)
  println("tenStream = " + tenStream.myString)
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
  println("streamOfFiniteStreams.take(42) = " + streamOfFiniteStreams.take(42).myString)
  println("** drop ")
  println("Empty.drop(1) = " + Ch05.Empty.drop(1).myString)
  println("fiveStream.drop(-1) = " + fiveStream.drop(-1).myString)
  println("fiveStream.drop(0) = " + fiveStream.drop(0).myString)
  println("fiveStream.drop(3) = " + fiveStream.drop(3).myString)
  println("fiveStream.drop(7) = " + fiveStream.drop(7).myString)
  println("identityStream.drop(10).take(42) = " + identityStream.drop(10).take(42).myString)
  println("squareStream.drop(10).take(42) = " + squareStream.drop(10).take(42).myString)

  println("** Exercise 5.3 **")
  println("Empty.takeWhile(_ => true) = " + Ch05.Empty.takeWhile(_ => true).myString)
  println("fiveStream.takeWhile(_%2==0) = " + fiveStream.takeWhile(_ % 2 == 0).myString)
  println("identityStream.takeWhile( n => (n%2==0) || (n<10) ) = "
    + identityStream.takeWhile(n => (n % 2 == 0) || (n < 10)).myString)
  println("squareStream.takeWhile( n => (n%2==0) || (n<10) ) = "
    + squareStream.takeWhile(n => (n % 2 == 0) || (n < 10)).myString)

  println("** Exercise 5.4 **")
  println("onesAndTwos.exists(_==1) = " + onesAndTwos.exists(_ == 1))
  // The following example shows that the existence function is not semi-decidable
  println("!!!! onesAndTwos.exists(_==2) = yields INFINITE LOOP WITHOUT THROWING ERROR (at least not within 3 minutes)")
  println("!!!! onesAndTwos contains 2 but the search nevertheless does not terminate.")
  //println("onesAndTwos.exists(_==2) = " + onesAndTwos.exists(_==2))
  println("fiveStream.forAll(_<6) = " + fiveStream.forAll(_ < 6))
  println("fiveStream.forAll(_%2==0) = " + fiveStream.forAll(_ % 2 == 0))
  println("identityStream.forAll(_<1000) = " + identityStream.forAll(_ < 1000))
  println("squareStream.forAll(_<10000) = " + squareStream.forAll(_ < 10000))

  println("** Exercise 5.5 **")
  println("Empty.takeWhileFoldRight(_ => true) = " + Ch05.Empty.takeWhileFoldRight(_ => true).myString)
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
  println("streamOfMultiples(5).take(20) = " + streamOfMultiples(5).take(20).myString)
  println("fiveStream.flatMap(n=>Ch05.Stream(n-1,n,n+1)) = "
    + fiveStream.flatMap(n => Ch05.Stream(n - 1, n, n + 1)).myString)
  println("fiveStream.drop(3).flatMap(streamOfMultiples).take(20) = "
    + fiveStream.drop(3).flatMap(streamOfMultiples).take(20).myString)
  println("squareStream.flatMap[Int](n=>Ch05.Stream[Int](n)).take(20) = " + squareStream.flatMap[Int](n => Ch05.Stream[Int](n)).take(20))
  println("squareStream.flatMap(streamOfMultiples).take(42) = " + squareStream.flatMap(streamOfMultiples).take(42).myString)
  println("squareStream.drop(3).flatMap(streamOfMultiples).take(20) = " + squareStream.drop(3).flatMap(streamOfMultiples).take(20))

  println("** Exercise 5.8 **")
  println("constant[Int](1).take(20) = " + Ch05.constant[Int](1).take(20).myString)
  println("constant[String](\"I like Scala.\").take(9) = " + Ch05.constant[String]("I like Scala.").take(9).myString)

  println("** Exercise 5.9 **")
  println("from(13).take(20) = " + Ch05.from(13).take(20).myString)
  println("from(-213).take(20) = " + Ch05.from(-213).take(20).myString)

  println("** Exercise 5.10 **")
  println("fibs().take(30) = " + Ch05.fibs().take(30).myString)

  println("** Exercise 5.11 **")
  println("unfold[Int,Int](0)(n=>Some((n,n+1))).take(20) = " + Ch05.unfold[Int, Int](0)(n => Some((n, n + 1))).take(20).myString)
  println("unfold[Int,Int](0)(n=>if (n<20) Some((n,n+1))else None) = " + Ch05.unfold[Int, Int](0)(n => if (n < 20) Some((n, n + 1)) else None).myString)
  println("unfold[Int,Int](0)(n=>if (n%2==0) Some((n,n+1))else None) = " + Ch05.unfold[Int, Int](0)(n => if (n % 2 == 0) Some((n, n + 1)) else None).myString)

  println("** Exercise 5.12 **")
  println("fibUnfold(100000000) = " + Ch05.fibUnfold(100000000).myString)

  println("** Exercise 5.13 **")
  println("** mapUnfold ")
  println("mapUnfold(tenStream)(n=>Some(n+n)) = " + tenStream.mapUnfold(n => Some(n + n)).myString)
  println("mapUnfold(fiveStream)(n=>streamOfMultiples(n)) = " + fiveStream.mapUnfold(n => Some(streamOfMultiples(n))))
  println("mapUnfold(ones)(n=>Some(n+n)).take(20) = " + ones.mapUnfold(n => Some(n + n)).take(20).myString)
  println("mapUnfold(onesAndTwos)(n=>Some(n+n)).take(20) = " + onesAndTwos.mapUnfold(n => Some(n + n)).take(20).myString)
  println("** takeUnfold ")
  println("Empty.takeUnfold(1) = " + Ch05.Empty.takeUnfold(1).myString)
  println("fiveStream.takeUnfold(-1) = " + fiveStream.takeUnfold(-1).myString)
  println("fiveStream.takeUnfold(0) = " + fiveStream.takeUnfold(0).myString)
  println("fiveStream.takeUnfold(3) = " + fiveStream.takeUnfold(3).myString)
  println("fiveStream.takeUnfold(7) = " + fiveStream.takeUnfold(7).myString)
  println("identityStream.takeUnfold(42) = " + identityStream.takeUnfold(42).myString)
  println("squareStream.takeUnfold(42) = " + squareStream.takeUnfold(42).myString)
  println("streamOfFiniteStreams.takeUnfold(21) = " + streamOfFiniteStreams.takeUnfold(21).myString)
  println("** takeWhileUnfold ")
  println("Empty.takeWhileUnfold(_ => true) = " + Ch05.Empty.takeWhileUnfold(_ => true).myString)
  println("fiveStream.takeWhileUnfold(_ % 2 == 0) = " + fiveStream.takeWhileUnfold(_ % 2 == 0).myString)
  println("identityStream.takeWhileUnfold(n => (n % 2 == 0) || (n < 10)) = "
    + identityStream.takeWhileUnfold(n => (n % 2 == 0) || (n < 10)).myString)
  println("squareStream.takeWhileUnfold(n => (n % 2 == 0) || (n < 10)) = "
    + squareStream.takeWhileUnfold(n => (n % 2 == 0) || (n < 10)).myString)
  println("** zipWith ")
  println("fiveStream.zipWith(tenStream)(n=>m=>n+m) = " + fiveStream.zipWith(tenStream)(n => m => n + m).myString)
  println("fiveStream.zipWith(tenStream)(n=>m=>n*m) = " + fiveStream.zipWith(tenStream)(n => m => n * m).myString)
  println("fiveStream.zipWith(tenStream)(n=>m=>n==m) = " + fiveStream.zipWith(tenStream)(n => m => n == m).myString)
  println("tenStream.zipWith(fiveStream)(n=>m=>n==m) = " + tenStream.zipWith(fiveStream)(n => m => n == m).myString)
  println("ones.zipWith(twos)(n=>m=>n+m).take(20) = " + ones.zipWith(twos)(n => m => n + m).take(20).myString)
  println("ones.zipWith(twos)(n=>m=>n==m).take(20) = " + ones.zipWith(twos)(n => m => n == m).take(20).myString)
  println("** zipAll ")
  println("fiveStream.zipAll(tenStream) = " + fiveStream.zipAll(tenStream).myString)
  println("fiveStream.zipAll(tenStream) = " + fiveStream.zipAll(tenStream).myString)
  println("ones.zipAll(twos).take(10) = " + ones.zipAll(twos).take(10).myString)
  println("identityStream.zipAll(squareStream).take(10) = " + identityStream.zipAll(squareStream).take(10).myString)

  println("** Exercise 5.14 **")
  println("** startsWith ")
  println("tenStream.startsWith(fiveStream) = " + tenStream.startsWith(fiveStream))
  println("fiveStream.startsWith(tenStream) = " + fiveStream.startsWith(tenStream))
  println("onesAndTwos.startsWith(ones) = *** INFINITE LOOP ***")
  println("onesAndTwos.startsWith(twos) = " + onesAndTwos.startsWith(twos))
  println("identityStream.startsWith(squareStream) = " + identityStream.startsWith(squareStream))

  println("** Exercise 5.15 **")
  println("** tails ")
  println("Empty.tails = " + Ch05.Empty.tails.myString)
  println("fiveStream.tails = " + fiveStream.tails.myString)
  println("ones.tails = " + ones.tails.take(10))
  println("onesAndTwos.tails.take(10).myString = " + onesAndTwos.tails.take(10))
  println("identityStream.tails.take(10) = " + identityStream.tails.take(10))

  println("** Exercise 5.16 **")

  println("** scanLeft ")
  println("Empty.scanLeft = " + Ch05.Stream.empty[Int].scanLeft[Int, Int](n => m => n + m)((0, 0)).myString)
  println("Stream(1).scanLeft = " + Ch05.Stream(1).scanLeft[Int, Int](n => m => n + m)(0, 0).myString)
  println("Stream(1,20).scanLeft[Int,Int](n=>m=>n+m)(0,0) = " + Ch05.Stream(1, 20).scanLeft[Int, Int](n => m => n + m)(0, 0).myString)
  println("Stream(1,20,300).scanLeft[Int,Int](n=>m=>n+m)(0,0) = "
    + Ch05.Stream(1, 20, 300).scanLeft[Int, Int](n => m => n + m)(0, 0).myString)
  println("Stream(1,20,300,4000,50000,600000,7000000,80000000,900000000).scanLeft[Int,Int](n=>m=>n+m)(0,0) = "
    + Ch05.Stream(1, 20, 300, 4000, 50000, 600000, 7000000, 80000000, 900000000).scanLeft[Int, Int](n => m => n + m)(0, 0).myString)
  println("Stream(\"a\",\"bc\",\"def\").scanLeft[String,Int]( s => n => s.length + n)((0,\"\")) = "
    + Ch05.Stream("a", "bc", "def").scanLeft[String, Int](s => n => s.length + n)((0, "")).myString)
  println("ones.scanLeft[Int,Int](n => m => n + m)((0,0)).take(10) = " + ones.scanLeft[Int, Int](n => m => n + m)((0, 0)).take(10).myString)
  println("ones.scanLeft[Int,Boolean]( n => p => (n % 2 == 1) && p)((true,1)).take(10) = " + ones.scanLeft[Int, Boolean](n => p => (n % 2 == 1) && p)((true, 1)).take(10).myString)
  println("identityStream.scanLeft[Int,Boolean]( n => p => (n % 7 < 6) && p)((true,1)).take(10) = " + identityStream.scanLeft[Int, Boolean](n => p => (n % 7 < 6) && p)((true, 1)).take(10).myString)
  println("identityStream.scanLeft[Int,Boolean]( n => p => (n % 7 < 6) != p)((true,1)).take(10) = " + identityStream.scanLeft[Int, Boolean](n => p => (n % 7 < 6) != p)((true, 1)).take(10).myString)

  println("** scanLeft2 ")
  println("Empty.scanLeft2 = " + Ch05.Stream.empty[Int].scanLeft2[Int, Int](n => m => n + m)((0, 0)).myString)
  println("Stream(1).scanLeft2 = " + Ch05.Stream(1).scanLeft2[Int, Int](n => m => n + m)(0, 0).myString)
  println("Stream(1,20).scanLeft2[Int,Int](n=>m=>n+m)(0,0) = " + Ch05.Stream(1, 20).scanLeft2[Int, Int](n => m => n + m)(0, 0).myString)
  println("Stream(1,20,300).scanLeft2[Int,Int](n=>m=>n+m)(0,0) = "
    + Ch05.Stream(1, 20, 300).scanLeft2[Int, Int](n => m => n + m)(0, 0).myString)
  println("Stream(1,20,300,4000,50000,600000,7000000,80000000,900000000).scanLeft2[Int,Int](n=>m=>n+m)(0,0) = "
    + Ch05.Stream(1, 20, 300, 4000, 50000, 600000, 7000000, 80000000, 900000000).scanLeft2[Int, Int](n => m => n + m)(0, 0).myString)
  println("Stream(\"a\",\"bc\",\"def\").scanLeft2[String,Int]( s => n => s.length + n)((0,\"\")) = "
    + Ch05.Stream("a", "bc", "def").scanLeft2[String, Int](s => n => s.length + n)((0, "")).myString)
  println("ones.scanLeft2[Int,Int](n => m => n + m)((0,0)).take(10) = " + ones.scanLeft2[Int, Int](n => m => n + m)((0, 0)).take(10).myString)
  println("ones.scanLeft2[Int,Boolean]( n => p => (n % 2 == 1) && p)((true,1)).take(10) = " + ones.scanLeft2[Int, Boolean](n => p => (n % 2 == 1) && p)((true, 1)).take(10).myString)
  println("identityStream.scanLeft2[Int,Boolean]( n => p => (n % 7 < 6) && p)((true,1)).take(10) = " + identityStream.scanLeft2[Int, Boolean](n => p => (n % 7 < 6) && p)((true, 1)).take(10).myString)
  println("identityStream.scanLeft2[Int,Boolean]( n => p => (n % 7 < 6) != p)((true,1)).take(10) = " + identityStream.scanLeft2[Int, Boolean](n => p => (n % 7 < 6) != p)((true, 1)).take(10).myString)

  println("** scanLeft3 ")
  println("Empty.scanLeft3 = " + Ch05.Stream.empty[Int].scanLeft3[Int, Int](n => m => n + m)((0, 0)).myString)

  println("** scanRightReverse ")
  println("Empty.scanRightReverse = " + Ch05.Stream.empty[Int].scanRightReverse[Int, Int](n => m => n + m)((0, 0)).myString)
  println("Stream(1).scanRightReverse = " + Ch05.Stream(1).scanRightReverse[Int, Int](n => m => n + m)(0, 0).myString)
  println("Stream(1,20).scanRightReverse[Int,Int](n=>m=>n+m)(0,0) = " + Ch05.Stream(1, 20).scanRightReverse[Int, Int](n => m => n + m)(0, 0).myString)
  println("Stream(1,20,300).scanRightReverse[Int,Int](n=>m=>n+m)(0,0) = "
    + Ch05.Stream(1, 20, 300).scanRightReverse[Int, Int](n => m => n + m)(0, 0).myString)
  println("Stream(1,20,300,4000,50000,600000,7000000,80000000,900000000).scanRightReverse[Int,Int](n=>m=>n+m)(0,0) = "
    + Ch05.Stream(1, 20, 300, 4000, 50000, 600000, 7000000, 80000000, 900000000).scanRightReverse[Int, Int](n => m => n + m)(0, 0).myString)
  println("Stream(\"a\",\"bc\",\"def\").scanRightReverse[String,Int]( s => n => s.length + n)((0,\"\")) = "
    + Ch05.Stream("a", "bc", "def").scanRightReverse[String, Int](s => n => s.length + n)((0, "")).myString)
  println("ones.scanRightReverse[Int,Int](n => m => n + m)((0,0)).take(10) = *** INFINITE LOOP ***")
  println("ones.scanRightReverse[Int,Boolean]( n => p => (n % 2 == 1) && p)((true,1)).take(10) = *** INFINITE LOOP ***")
  println("identityStream.scanRightReverse[Int,Boolean]( n => p => (n % 7 < 6) && p)((true,1)).take(10) = *** INFINITE LOOP ***")
  println("identityStream.scanRightReverse[Int,Boolean]( n => p => (n % 7 < 6) != p)((true,1)).take(10) = *** INFINITE LOOP ***")

  println("** scanRightUnfoldLeft ")
  println("Empty.scanRightUnfoldLeft = " + Ch05.Stream.empty[Int].scanRightUnfoldReverse[Int, Int](n => m => n + m)((0, 0)).myString)
  println("Stream(1).scanRightUnfoldLeft = " + Ch05.Stream(1).scanRightUnfoldReverse[Int, Int](n => m => n + m)(0, 0).myString)
  println("Stream(1,20).scanRightUnfoldLeft[Int,Int](n=>m=>n+m)(0,0) = " + Ch05.Stream(1, 20).scanRightUnfoldReverse[Int, Int](n => m => n + m)(0, 0).myString)
  println("Stream(1,20,300).scanRightUnfoldLeft[Int,Int](n=>m=>n+m)(0,0) = "
    + Ch05.Stream(1, 20, 300).scanRightUnfoldReverse[Int, Int](n => m => n + m)(0, 0).myString)
  println("Stream(1,20,300,4000,50000,600000,7000000,80000000,900000000).scanRightUnfoldLeft[Int,Int](n=>m=>n+m)(0,0) = "
    + Ch05.Stream(1, 20, 300, 4000, 50000, 600000, 7000000, 80000000, 900000000).scanRightUnfoldReverse[Int, Int](n => m => n + m)(0, 0).myString)
  println("Stream(\"a\",\"bc\",\"def\").scanRightUnfoldLeft[String,Int]( s => n => s.length + n)((0,\"\")) = "
    + Ch05.Stream("a", "bc", "def").scanRightUnfoldReverse[String, Int](s => n => s.length + n)((0, "")).myString)
  println("ones.scanRightUnfoldLeft[Int,Int](n => m => n + m)((0,0)).take(10) = *** INFINITE LOOP ***")
  println("ones.scanRightUnfoldLeft[Int,Boolean]( n => p => (n % 2 == 1) && p)((true,1)).take(10) = *** INFINITE LOOP ***")
  println("identityStream.scanRightUnfoldLeft[Int,Boolean]( n => p => (n % 7 < 6) && p)((true,1)).take(10) = *** INFINITE LOOP ***")
  println("identityStream.scanRightUnfoldLeft[Int,Boolean]( n => p => (n % 7 < 6) != p)((true,1)).take(10) = *** INFINITE LOOP ***")

  println("**** Additional Experiments: the fun starts ****")

  println("Stream.recApply(0)(n=>n+1).take(10) = "+Ch05.Stream.recApply(0)(n=>n+1).take(10).myString)
  println("Stream.recApply(\"*\")(s=>s+s).take(10) = "+Ch05.Stream.recApply("*")(s=>s+s).take(10).myString)
  println("** reverse ")
  println("Empty.reverse) = " + Ch05.Empty.reverse.myString)
  println("fiveStream.reverse = " + fiveStream.reverse.myString)
  println("tenStream.reverse = " + tenStream.reverse.myString)

  println("** Existence does not find the witness ")
  println("Stream(ones).foldRightLazy[Int](0)((s,n)=>if (s.exists(_==2)) n else n) = *** INFINITE LOOP ***")

  println("** streamAppend **")
  println("Empty.streamAppend[Int](Empty) = " + Ch05.Empty.streamAppend[Int](Ch05.Empty).myString)
  println("Empty.streamAppend[String](Stream(Stream(\"a\"))) = "
    + Ch05.Empty.streamAppend[String](Ch05.Stream(Ch05.Stream("a"))).myString)
  println("Empty.streamAppend[Int](Stream(oneStream)) = "
    + Ch05.Empty.streamAppend[Int](Ch05.Stream(oneStream)).myString)
  println("oneStream.streamAppend[Int](Stream(oneStream)) = "
    + oneStream.streamAppend[Int](Ch05.Stream(oneStream)).myString)
  println("oneStream.streamAppend[Int](Ch05.Stream(fiveStream,tenStream,oneStream)) = "
    + oneStream.streamAppend[Int](Ch05.Stream(fiveStream, tenStream, oneStream)).myString)
  println("identityStream.streamAppend[Int](Ch05.Stream(fiveStream,tenStream,oneStream).take(20)) = "
    + identityStream.streamAppend[Int](Ch05.Stream(fiveStream, tenStream, oneStream)).take(20).myString)
  println("Empty.streamAppend[Int](streamOfFiniteStreams).take(0) = "
    + Ch05.Empty.streamAppend[Int](streamOfFiniteStreams).take(0).myString)
  println("Empty.streamAppend[Int](streamOfInfiniteStreams).take(20) = "
    + Ch05.Empty.streamAppend[Int](streamOfInfiniteStreams).take(20).myString)
  println("** streamCons **")
  println("fiveStream.streamConsFoldRight(Ch05.Stream(tenStream.drop(2), oneStream, fiveStream.drop(1), oneStream, fiveStream, tenStream)) = "
    + fiveStream.streamCons(Ch05.Stream(tenStream.drop(2), oneStream, fiveStream.drop(1), oneStream, fiveStream, tenStream)).myString)
  println("identityStream.streamConsFoldRight(streamOfInfiniteStreams) = "
    + identityStream.streamCons(streamOfInfiniteStreams))

  println("***** Done ***** ")

}
