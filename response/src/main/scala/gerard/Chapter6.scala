package gerard

object Chapter6 {

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
    // new range 0... Int.maxValue
    // old range Int.MinValue... Int.maxValue
    val (next, nextRng) = rng.nextInt
    (next / 2) - Int.MinValue / 2 -> nextRng
  }

  // 6.2 Write a function to generate a Double between 0 and 1, not including 1.
  // Note: You can use Int.MaxValue to obtain the maximum positive integer value, and you
  // can use x.toDouble to convert an x: Int to a Double.
  def double(rng: RNG): (Double, RNG) = {
    val (next, nextRng) = nonNegativeInt(rng)
    next.toDouble / (Int.MaxValue + 1.0) -> nextRng
  }

  // 6.3 Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
  // (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve already written.
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (next, nextRng) = nonNegativeInt(rng)
    val (next2, nextRng2) = double(nextRng)
    (next -> next2) -> nextRng2
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (next, nextRng) = intDouble(rng)
    next.swap -> nextRng
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    (d1, d2, d3) -> r3
  }

  // 6.4 Write a function to generate a list of random integers.
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      Nil -> rng
    } else {
      val (next, nextRng) = int(rng)
      val (intss, finalRng) = ints(count - 1)(nextRng)
      (next :: intss) -> finalRng
    }
  }


  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)


  // 6.5 Use map to reimplement double in a more elegant way. See exercise 6.2.
  def mapDouble(rng: RNG): (Double, RNG) = {
    map(int)(_.toDouble / (Int.MaxValue + 1.0))(rng)
  }

  // 6.6  Write the implementation of map2 based on the following signature.
  // This function takes two actions, ra and rb, and a function f for combining their results,
  // and returns a new action that combines them:
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng: RNG =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      f(a, b) -> rng3
  }


  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  lazy val randIntDouble: Rand[(Int, Double)] = both(int, double)
  lazy val randDoubleInt: Rand[(Double, Int)] = both(double, int)


  // 6.7 Hard: If you can combine two RNG transitions, you should be able to combine a whole list of them.
  // Implement sequence for combining a List of transitions into a single transition.
  // Use it to reimplement the ints function you wrote before. For the latter, you can use the standard library
  // function List.fill(n)(x) to make a list with x repeated n times.

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = map {
    rng: RNG =>
      fs.foldLeft(List.empty[A] -> rng) {
        case ((acc, r), f) =>
          val (a, rngNext) = f(r)
          (a :: acc) -> rngNext
      }
  }(l => l.reverse) // wrapped in map such that we reverse the list only once

  def seqInts(count: Int)(rng: RNG): (List[Int], RNG) = {
    val rngs = List.fill(count)(int)
    sequence(rngs)(rng)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng => val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  // 6.8 Implement flatMap, and then use it to implement nonNegativeLessThan.
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng: RNG =>
      val (a, rngNext) = f(rng)
      g(a)(rngNext)
  }

  def nonNegativeLessThan2(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) {
      i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) {
          rng => (mod, rng)
        }
        else {
          rng => nonNegativeLessThan2(n)(rng)
        }
    }
  }


  // 6.9 Reimplement map and map2 in terms of flatMap. The fact that this is possible is what
  // we’re referring to when we say that flatMap is more powerful than map and map2.
  def mapUsingFM[A, B](s: Rand[A])(f: A => B) = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2FM[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra) {
      a => map(rb) {
        b => f(a, b)
      }
    }
  }

  case class State[S, +A](run: S => (A, S)) {

    def flatMap[B](f: A => State[S, B]): State[S, B] = State {
      s =>
        val (a, sNext) = run(s)
        f(a).run(sNext)
    }

    def map[B](f: A => B): State[S, B] = {
      flatMap(a => State.unit(f(a)))
    }
  }

  object State {
    def unit[A, S](a: A) = State[S, A] {
      (s: S) => (a, s)
    }

    def get[S]: State[S, S] = State(s => s -> s)

    def set[S](s: S): State[S, Unit] = State(_ => () -> s)
  }

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  type MachineState = State[Machine, (Int, Int)]

  // question: how would you write that with the getter / setters?
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State {
    machine =>

      def step(m: Machine, input: Input): Machine = (input, m) match {
        case (_, m@Machine(_, 0, _))               =>
          m
        case (Coin, m@Machine(true, _, coins0))    =>
          m.copy(locked = false, coins = coins0 + 1)
        case (Turn, m@Machine(false, candies0, _)) =>
          m.copy(locked = true, candies = candies0 - 1)
        case (_, m)                                =>
          m
      }

      val end = inputs.foldLeft(machine)(step)
      (end.candies, end.coins) -> end
  }

  def main(args: Array[String]) {
    println(s"ints: ${ints(5)(new SimpleRNG(1))}")
    println(s"seqints: ${seqInts(5)(new SimpleRNG(1))}")
    println(s"nonNegativeLessThan2: ${nonNegativeLessThan2(42)(new SimpleRNG(1))}")
    val machine = Machine(locked = true, 5, 10)
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val end = simulateMachine(inputs).run(machine)
    println(s"simulated machine ${end}")
  }
}
