object Ch06 {

  trait RNG {
     def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    final def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }


  // 6.1 Write a function that uses RNG.nextInt to generate a random integer between 0 and
  // Int.maxValue (inclusive). Make sure to handle the corner case when nextInt returns
  // Int.MinValue, which doesn’t have a non-negative counterpart.
  final def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (x, y) => (if (x < 0) x - Int.MinValue else x, y)
  }

  // 6.2 Write a function to generate a Double between 0 and 1, not including 1.
  // Note: You can use Int.MaxValue to obtain the maximum positive integer value, and you
  // can use x.toDouble to convert an x: Int to a Double.
  final def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (x, y) => (0 - x.toDouble / Int.MinValue, y)
  }


  // 6.3 Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
  // (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve already written.
  // QUESTION
  // Do we require the integer to be non-negative and the double to be in [0;1[ ?
  // If not then it does not give much sense to resue the functions from 6.1 and 6.2
  // Therefore I assume yes
  final def intDouble(rng: RNG): ((Int, Double), RNG) = nonNegativeInt(rng) match {
    case (n, rng1) => ((n, double(rng1)._1), rng1)
  }

  final def doubleInt(rng: RNG): ((Double, Int), RNG) = intDouble(rng) match {
    case ((n, d), rng1) => ((d, n), rng1)
  }

  final def double3(rng: RNG): ((Double, Double, Double), RNG) = double(rng) match {
    case (d1, rng1) => double(rng1) match {
      case (d2, rng2) => double(rng2) match {
        case (d3, rng3) => ((d1, d2, d3), rng3)
      }
    }
  }

  // 6.4 Write a function to generate a list of random integers.

  final def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
     def createNext(ir: (Int, RNG)): Option[((Int, RNG), (Int, RNG))] = ir match {
      case (i, r) => if (count < i) None
      else r.nextInt match {
        case (n: Int, r2: RNG) => Some[((Int, RNG), (Int, RNG))](((n, r2), (i + 1, r2)))
      }
    }
    // We use onfoldReverse in order to have the last RNG as head of the list
    // so that we can simply return head._2
    val intRngs: List[(Int, RNG)] = Ch05.unfoldReverse[(Int, RNG), (Int, RNG)](Ch05.Empty)((1, SimpleRNG(0)))(createNext).toList

    intRngs match {
      case List.Cons(n, tail) => (List.map(intRngs)(x => x._1), n._2)
      case _ => (List.Nil, rng)
    }
  }


  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  final def unit[A](a: A): Rand[A] = rng => (a, rng)

  final def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  final def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)


  // 6.5 Use map to reimplement double in a more elegant way. See exercise 6.2.
  final def doubleMap: Rand[Double] = map(nonNegativeInt)(x => 0 - x.toDouble / Int.MinValue)


  // 6.6  Write the implementation of map2 based on the following signature.
  // This function takes two actions, ra and rb, and a function f for combining their results,
  // and returns a new action that combines them:
  final def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }


  final def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)


  // 6.7 Hard: If you can combine two RNG transitions, you should be able to combine a whole list of them.
  // Implement sequence for combining a List of transitions into a single transition.
  // Use it to reimplement the ints function you wrote before. For the latter, you can use the standard library
  // function List.fill(n)(x) to make a list with x repeated n times.

  //  We set sequence[A](Nil) = rng => (Nil,rng)
  final def sequence[A](fs: List[Rand[A]]): Rand[List[A]]
  = List.foldLeft[Rand[A], Rand[List[A]]](fs, rng => (List.Nil, rng))(ra => rla => map2(ra, rla)(List.Cons(_, _)))

  final def intsSequence(count: Int): Rand[List[Int]] = sequence[Int](List.fill[Rand[Int]](count)(int))

  final def nonNegativeLessThan(n: Int): Rand[Int] = {
    rng => val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        (mod, rng2)
      else nonNegativeLessThan(n)(rng)
  }

  // 6.8 Implement flatMap, and then use it to implement nonNegativeLessThan.
  final def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  final def nonNegativeLessThanFlatMap(n: Int): Rand[Int]
  = flatMap[Int, Int](nonNegativeInt)(i => rng => {
    val mod = i % n
    if (i + (n - 1) - mod >= 0) (mod, rng) else nonNegativeLessThanFlatMap(n)(rng)
  })

  // 6.9 Reimplement map and map2 in terms of flatMap. The fact that this is possible is what
  // we’re referring to when we say that flatMap is more powerful than map and map2.
  final def map2Flat[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C]
  = flatMap[(A, B), C](rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    ((a, b), rng3)
  })(x => x match {
    case (a, b) => (r => (f(a, b), r))
  })

  final def sequenceFlat[A](fs: List[Rand[A]]): Rand[List[A]]
  = List.foldLeft[Rand[A], Rand[List[A]]](fs, rng => (List.Nil, rng))(ra => rla => map2Flat(ra, rla)(List.Cons(_, _)))

  final def intsSequenceFlat(count: Int): Rand[List[Int]] = sequenceFlat[Int](List.fill[Rand[Int]](count)(int))

  final def mapFlat[A, B](s: Rand[A])(f: A => B): Rand[B] = map2Flat[A, A, B](s, s)((a1, a2) => f(a1))

  final def doubleMapFlat: Rand[Double] = mapFlat(nonNegativeInt)(x => 0 - x.toDouble / Int.MinValue)


// Chapter 6.5 A general state action data type

  case class State[S, +A](run: S => (A, S)) {
    final def flatMap[B](g: A => State[S, B]): State[S, B] = {
//      val me = this
      val res = State[S, B](state => {
        val (a, state2): (A, S) = this.run(state)
        g(a).run(state2)
      })
      res
    }

  }

  type RandState[A] = State[RNG, A]

  final def nonNegativeIntRandi: RandState[Int] = State[Ch06.RNG, Int](nonNegativeInt)
  final def doubleRandi: RandState[Double] = State[Ch06.RNG, Double](doubleMap)

  final def nonNegativeLessThanState(n: Int): RandState[Int]
  = nonNegativeIntRandi.flatMap[Int](i => {
    val mod = i % n
    if (i + (n - 1) - mod >= 0) State[Ch06.RNG, Int](rng => (mod, rng))
    else this.nonNegativeLessThanState(n)
  })


}


object nith_Chapter_06 extends App {

  val rng0: Ch06.RNG = Ch06.SimpleRNG(0)
  val SimpleRNGstream: (Long => Ch05.Stream[Int]) = fstSeed => Ch05.unfold[Int, Ch06.RNG](Ch06.SimpleRNG(fstSeed))(rng => Some(rng.nextInt))

  println("****** Chapter_06 ******")
  println("Int.MinValue = %s".format(Int.MinValue))
  println("Int.MaxValue =  %s".format(Int.MaxValue))
  println("Long.MinValue = %s".format(Long.MinValue))
  println("Long.MaxValue =  %s".format(Long.MaxValue))
  println("rng0.nextInt = %s".format(rng0.nextInt))
  println("SimpleRNG(Long.MinValue).nextInt = %s".format(Ch06.SimpleRNG(Long.MinValue).nextInt))
  println("SimpleRNG(-1).nextInt = %s".format(Ch06.SimpleRNG(-1).nextInt))
  println("SimpleRNG(Long.MaxValue).nextInt = %s".format(Ch06.SimpleRNG(Long.MaxValue).nextInt))
  println("SimpleRNGstream(Long.MinValue).take(10) = %s".format(SimpleRNGstream(Long.MinValue).take(10).myString))
  println("SimpleRNGstream(0).take(10) = %s".format(SimpleRNGstream(0).take(10).myString))
  println("SimpleRNGstream(42).take(10) = %s".format(SimpleRNGstream(42).take(10).myString))

  println("** Exercise 6.1 **")
  println("unfold(rng0)(rng => Some(nonNegativeInt(rng))).take(20)\n  = %s"
    .format(Ch05.unfold[Int, Ch06.RNG](rng0)(rng => Some(Ch06.nonNegativeInt(rng))).take(20).myString))

  println("** Exercise 6.2 **")
  println("unfold(rng0)(rng => Some(double(rng))).take(20)\n  = %s"
    .format(Ch05.unfold[Double, Ch06.RNG](rng0)(rng => Some(Ch06.double(rng))).take(20).myString))

  println("** Exercise 6.3 **")
  println("unfold(SimpleRNG(-1))(rng => Some(intDouble(rng))).take(10)\n  = %s"
    .format(Ch05.unfold[(Int, Double), Ch06.RNG](Ch06.SimpleRNG(-1))(rng => Some(Ch06.intDouble(rng))).take(10).myString))
  println("unfold(SimpleRNG(-1))(rng => Some(doubleInt(rng))).take(10)\n  = %s"
    .format(Ch05.unfold[(Double, Int), Ch06.RNG](Ch06.SimpleRNG(-1))(rng => Some(Ch06.doubleInt(rng))).take(10).myString))
  println("unfold(SimpleRNG(-1))(rng => Some(double3(rng))).take(10)\n  = %s"
    .format(Ch05.unfold[(Double, Double, Double), Ch06.RNG](Ch06.SimpleRNG(-1))(rng => Some(Ch06.double3(rng))).take(10).myString))

  println("** Exercise 6.4 **")
  println("ints(-1)(rng0) = %s".format(Ch06.ints(-1)(rng0)))
  println("ints(0)(rng0) = %s".format(Ch06.ints(0)(rng0)))
  println("ints(1)(rng0) = %s".format(Ch06.ints(1)(rng0)))
  println("ints(3)(rng0) = %s".format(Ch06.ints(3)(rng0)))
  println("ints(10)(rng0) = %s".format(Ch06.ints(10)(rng0)))

  println("** Exercise 6.5 **")
  println("unfold(rng0)(rng => Some(double(rng))).take(20)\n  = %s"
    .format(Ch05.unfold[Double, Ch06.RNG](rng0)(rng => Some(Ch06.double(rng))).take(10).myString))
  println("unfold2(rng0)(3)(doubleMap)\n  = %s"
    .format(Ch05.unfold2[Double, Ch06.RNG](10)(rng0)(Ch06.doubleMap).myString))

  println("** Exercise 6.7 **")
  println("intsSequence(-1)(rng0) = %s".format(Ch06.intsSequence(-1)(rng0)))
  println("intsSequence(0)(rng0) = %s".format(Ch06.intsSequence(0)(rng0)))
  println("intsSequence(1)(rng0) = %s".format(Ch06.intsSequence(1)(rng0)))
  println("intsSequence(3)(rng0) = %s".format(Ch06.intsSequence(3)(rng0)))
  println("intsSequence(10)(rng0) = %s".format(Ch06.intsSequence(10)(rng0)))

  println("** Exercise 6.8 **")
  println("unfold2(20)(rng0)(nonNegativeLessThan(20)        = %s"
    .format(Ch05.unfold2[Int, Ch06.RNG](20)(rng0)(Ch06.nonNegativeLessThan(20)).myString))
  println("unfold2(20)(rng0)(nonNegativeLessThanFlatMap(20) = %s"
    .format(Ch05.unfold2[Int, Ch06.RNG](20)(rng0)(Ch06.nonNegativeLessThanFlatMap(20)).myString))

  println("** Exercise 6.9 **")
  println("intsSequenceFlat(10)(rng0) = %s".format(Ch06.intsSequenceFlat(10)(rng0)))
  println("unfold2(rng0)(3)(doubleMap)\n  = %s"
    .format(Ch05.unfold2[Double, Ch06.RNG](10)(rng0)(Ch06.doubleMap).myString))
  println("unfold2(rng0)(3)(doubleMapFlat)\n  = %s"
    .format(Ch05.unfold2[Double, Ch06.RNG](10)(rng0)(Ch06.doubleMapFlat).myString))

  println("** Exercise 6.10 **")
  println("unfold2(10)(rng0)(rng => nonNegativeInt(rng))          = %s"
    .format(Ch05.unfold2[Int, Ch06.RNG](10)(rng0)(rng => Ch06.nonNegativeInt(rng)).myString))
  println("unfold2(10)(rng0)(rng => nonNegativeIntRandi.run(rng)) = %s"
    .format(Ch05.unfold2[Int, Ch06.RNG](10)(rng0)(rng => Ch06.nonNegativeIntRandi.run(rng)).myString))
  println("unfold2(10)(rng0)(rng => double(rng))          = %s"
    .format(Ch05.unfold2[Double, Ch06.RNG](10)(rng0)(rng => Ch06.double(rng)).myString))
  println("unfold2(10)(rng0)(rng => doubleRandi.run(rng)) = %s"
    .format(Ch05.unfold2[Double, Ch06.RNG](10)(rng0)(rng => Ch06.doubleRandi.run(rng)).myString))
  println("unfold2(20)(rng0)(nonNegativeLessThan(20)                       = %s"
    .format(Ch05.unfold2[Int, Ch06.RNG](20)(rng0)(Ch06.nonNegativeLessThan(20)).myString))
  println("unfold2(20)(rng0)(rng => nonNegativeLessThanState(20).run(rng)) = %s"
    .format(Ch05.unfold2[Int, Ch06.RNG](20)(rng0)(rng => Ch06.nonNegativeLessThanState(20).run(rng)).myString))

  println("***** Done ***** ")

}
