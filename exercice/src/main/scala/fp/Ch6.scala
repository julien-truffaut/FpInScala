package fp


object Ch6 {

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
  def nonNegativeInt(rng: RNG): (Int, RNG) = ???

  // 6.2 Write a function to generate a Double between 0 and 1, not including 1.
  // Note: You can use Int.MaxValue to obtain the maximum positive integer value, and you
  // can use x.toDouble to convert an x: Int to a Double.
  def double(rng: RNG): (Double, RNG) = ???

  // 6.3 Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
  // (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve already written.
  def intDouble(rng: RNG): ((Int,Double), RNG) = ???
  def doubleInt(rng: RNG): ((Double,Int), RNG) = ???
  def double3(rng: RNG): ((Double,Double,Double), RNG) = ???

  // 6.4 Write a function to generate a list of random integers.
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???


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

}
