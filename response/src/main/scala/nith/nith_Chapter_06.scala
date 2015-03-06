object Ch06 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }


  // 6.1 Write a function that uses RNG.nextInt to generate a random integer between 0 and
  // Int.maxValue (inclusive). Make sure to handle the corner case when nextInt returns
  // Int.MinValue, which doesn’t have a non-negative counterpart.
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (x, y) => (if (x < 0) x - Int.MinValue else x, y)
  }

  // 6.2 Write a function to generate a Double between 0 and 1, not including 1.
  // Note: You can use Int.MaxValue to obtain the maximum positive integer value, and you
  // can use x.toDouble to convert an x: Int to a Double.
  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (x, y) => (0 - x.toDouble / Int.MinValue, y)
  }


  // 6.3 Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
  // (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve already written.
  // QUESTION
  // Do we require the integer to be non-negative and the double to be in [0;1[ ?
  // If not then it does not give much sense to resue the functions from 6.1 and 6.2
  // Therefore I assume yes
  def intDouble(rng: RNG): ((Int, Double), RNG) = nonNegativeInt(rng) match {
    case (n, rng1) => ((n, double(rng1)._1), rng1)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = intDouble(rng) match {
    case ((n, d), rng1) => ((d, n), rng1)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = double(rng) match {
    case (d1, rng1) => double(rng1) match {
      case (d2, rng2) => double(rng2) match {
        case (d3, rng3) => ((d1, d2, d3), rng3)
      }
    }
  }

  // 6.4 Write a function to generate a list of random integers.

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
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


  /*

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)


  // 6.5 Use map to reimplement double in a more elegant way. See exercise 6.2.


  // 6.6  Write the implementation of map2 based on the following signature.
  // This function takes two actions, ra and rb, and a function f for combining their results,
  // and returns a new action that combines them:
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???


  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)


  // 6.7 Hard: If you can combine two RNG transitions, you should be able to combine a whole list of them.
  // Implement sequence for combining a List of transitions into a single transition.
  // Use it to reimplement the ints function you wrote before. For the latter, you can use the standard library
  // function List.fill(n)(x) to make a list with x repeated n times.

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???


  def nonNegativeLessThan(n: Int): Rand[Int] = { rng => val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  // 6.8 Implement flatMap, and then use it to implement nonNegativeLessThan.
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

  // 6.9 Reimplement map and map2 in terms of flatMap. The fact that this is possible is what
  // we’re referring to when we say that flatMap is more powerful than map and map2.


  // TODO add missing exercices

  */
}


object nith_Chapter_06 extends App {

  val SimpleRNGstream: (Long => Ch05.Stream[Int]) = fstSeed => Ch05.unfold[Int, Ch06.RNG](Ch06.SimpleRNG(fstSeed))(rng => Some(rng.nextInt))

  println("****** Chapter_06 ******")
  println("Int.MinValue = %s".format(Int.MinValue))
  println("Int.MaxValue =  %s".format(Int.MaxValue))
  println("Long.MinValue = %s".format(Long.MinValue))
  println("Long.MaxValue =  %s".format(Long.MaxValue))
  println("SimpleRNG(0).nextInt = %s".format(Ch06.SimpleRNG(0).nextInt))
  println("SimpleRNG(Long.MinValue).nextInt = %s".format(Ch06.SimpleRNG(Long.MinValue).nextInt))
  println("SimpleRNG(-1).nextInt = %s".format(Ch06.SimpleRNG(-1).nextInt))
  println("SimpleRNG(Long.MaxValue).nextInt = %s".format(Ch06.SimpleRNG(Long.MaxValue).nextInt))
  println("SimpleRNGstream(Long.MinValue).take(10) = %s".format(SimpleRNGstream(Long.MinValue).take(10).myString))
  println("SimpleRNGstream(0).take(10) = %s".format(SimpleRNGstream(0).take(10).myString))
  println("SimpleRNGstream(42).take(10) = %s".format(SimpleRNGstream(42).take(10).myString))

  println("** Exercise 6.1 **")
  println("unfold(SimpleRNG(0))(rng => Some(nonNegativeInt(rng))).take(20)\n  = %s"
    .format(Ch05.unfold[Int, Ch06.RNG](Ch06.SimpleRNG(0))(rng => Some(Ch06.nonNegativeInt(rng))).take(20).myString))

  println("** Exercise 6.2 **")
  println("unfold(SimpleRNG(0))(rng => Some(double(rng))).take(20)\n  = %s"
    .format(Ch05.unfold[Double, Ch06.RNG](Ch06.SimpleRNG(0))(rng => Some(Ch06.double(rng))).take(20).myString))

  println("** Exercise 6.3 **")
  println("unfold(SimpleRNG(-1))(rng => Some(intDouble(rng))).take(10)\n  = %s"
    .format(Ch05.unfold[(Int, Double), Ch06.RNG](Ch06.SimpleRNG(-1))(rng => Some(Ch06.intDouble(rng))).take(10).myString))
  println("unfold(SimpleRNG(-1))(rng => Some(doubleInt(rng))).take(10)\n  = %s"
    .format(Ch05.unfold[(Double, Int), Ch06.RNG](Ch06.SimpleRNG(-1))(rng => Some(Ch06.doubleInt(rng))).take(10).myString))
  println("unfold(SimpleRNG(-1))(rng => Some(double3(rng))).take(10)\n  = %s"
    .format(Ch05.unfold[(Double, Double, Double), Ch06.RNG](Ch06.SimpleRNG(-1))(rng => Some(Ch06.double3(rng))).take(10).myString))

  println("** Exercise 6.4 **")
  println("ints(-1)(SimpleRNG(0)) = %s".format(Ch06.ints(-1)(Ch06.SimpleRNG(0))))
  println("ints(0)(SimpleRNG(0)) = %s".format(Ch06.ints(0)(Ch06.SimpleRNG(0))))
  println("ints(1)(SimpleRNG(0)) = %s".format(Ch06.ints(1)(Ch06.SimpleRNG(0))))
  println("ints(3)(SimpleRNG(0)) = %s".format(Ch06.ints(3)(Ch06.SimpleRNG(0))))
  println("ints(10)(SimpleRNG(0)) = %s".format(Ch06.ints(10)(Ch06.SimpleRNG(0))))

  println("***** Done ***** ")

}
