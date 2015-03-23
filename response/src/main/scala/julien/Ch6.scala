package julien

package fp

import fp.Ch6.{SimpleRNG, RNG}

import scala.annotation.tailrec


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
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if(i == Int.MinValue) nonNegativeInt(rng2)
    else (i.abs, rng2)
  }


  // 6.2 Write a function to generate a Double between 0 and 1, not including 1.
  // Note: You can use Int.MaxValue to obtain the maximum positive integer value, and you
  // can use x.toDouble to convert an x: Int to a Double.
  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    ((i.toDouble / Int.MinValue).abs, rng2)
  }

  // 6.3 Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
  // (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve already written.
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = ???
  def double3(rng: RNG): ((Double,Double,Double), RNG) = ???

  // 6.4 Write a function to generate a list of random integers.
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(n: Int, acc: List[Int], r: RNG): (List[Int], RNG) = {
      if(n <= 0) (acc, rng)
      else {
        val (i, rng2) = rng.nextInt
        loop(n - 1, i :: acc, rng2)
      }
    }
    loop(count, List.empty, rng)
  }


  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  val nonNegativeInt2: Rand[Int] = map(int)(_.abs)


  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)


  // 6.5 Use map to reimplement double in a more elegant way. See exercise 6.2.


  // 6.6  Write the implementation of map2 based on the following signature.
  // This function takes two actions, ra and rb, and a function f for combining their results,
  // and returns a new action that combines them:
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng: RNG =>
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a,b), rng3)
  }

  val _intDouble: Rand[(Int,Double)] = map2(int, double){ case (i, d) => (i, d) }


  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)


  // 6.7 Hard: If you can combine two RNG transitions, you should be able to combine a whole list of them.
  // Implement sequence for combining a List of transitions into a single transition.
  // Use it to reimplement the ints function you wrote before. For the latter, you can use the standard library
  // function List.fill(n)(x) to make a list with x repeated n times.

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng: RNG =>
    @tailrec
    def loop(rs: List[Rand[A]], acc: List[A], r: RNG): (List[A], RNG) = rs match {
      case Nil     => (acc.reverse, r)
      case x :: xs =>
        val (a, rng2) = x(rng)
        loop(xs, a :: acc, rng2)
    }

    loop(fs, List.empty, rng)
  }

  sequence(List(int, int, nonNegativeEven, nonNegativeInt2))


  def nonNegativeLessThan(n: Int): Rand[Int] = { rng => val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  // 6.8 Implement flatMap, and then use it to implement nonNegativeLessThan.
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng: RNG =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }



  // 6.9 Reimplement map and map2 in terms of flatMap. The fact that this is possible is what
  // we’re referring to when we say that flatMap is more powerful than map and map2.



  // TODO add missing exercices



}

object StateEx extends App {


  case class State[S,+A](run: S => (A,S)){

    def map[B](f: A => B): State[S, B] =
      State[S, B]{s: S =>
        val (a, s2) = run(s)
        (f(a), s2)
      }

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State[S, B]{ s: S =>
        val (a, s2) = run(s)
        val innerState = f(a) : State[S, B]
        innerState.run(s2)
      }

    def map2[B, C](other: State[S, B])(f: (A, B) => C): State[S ,C] =
      State[S, C]{ s: S =>
        val (a, s2) = run(s)
        val (b, s3) = other.run(s2)
        (f(a, b), s3)
      }

  }



  val increment = State[Int, Unit](i => () -> (i +1) )

  //increment.run(2) == ( (), 3 )

  val plus2: State[Int, Unit] = increment.flatMap( _ => increment)






  type Rand[A] = State[RNG, A]


  val int: Rand[Int] = State(_.nextInt)

  val str: Rand[String] = int.map(_.toString)

  val seed: RNG = SimpleRNG(0)

  println( int.run(seed) )

  val (rand1, newRng) = int.run(seed)

  println(newRng.nextInt)

  println( str.run(seed) )

  def constant[S, A](a: A): State[S, A] = State[S, A](s => (a, s) )

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] =
    states.foldRight(constant[S, List[A]](Nil))((a, acc) => a.map2(acc)( (a, as) => a :: as))

  def sequence2[S, A](states: List[State[S, A]]): State[S, List[A]] = states match {
    case Nil => constant(Nil)
    case x :: xs =>
      State[S, List[A]] { s =>
        val (a, s2) = x.run(s)
        val (b, s3) = sequence2(xs).run(s2)
        (a :: b, s3)
      }
  }



  def test[S](s: S*): String = s.map(_.toString).mkString(",")

  //  sequence(List(int)) // int : State[RNG, Int]

  println( test("plop") )
  println( test("plop", "hello") )

  List(1,2,3,4,5).foldLeft(0)((acc, a) => acc + a)

  val smallInt: Rand[Int] = int.map(i => i % 100)

  println(sequence(List(smallInt, smallInt,smallInt)).run(seed))


}
