import Ch04_Option.{None, Option, Some}
object Ch06 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    final def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
//      util.log("...SimpleRNG.nextInt: seed="+seed+"  n="+n)
      (n, nextRNG)
    }
  }


  // 6.1 Write aPar function that uses RNG.nextInt to generate aPar random integer between 0 and
  // Int.maxValue (inclusive). Make sure to handle the corner case when nextInt returns
  // Int.MinValue, which doesn’t have aPar non-negative counterpart.
  final def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (x, y) => (if (x < 0) x - Int.MinValue else x, y)
  }

  // 6.2 Write aPar function to generate aPar Double between 0 and 1, not including 1.
  // Note: You can use Int.MaxValue to obtain the maximum positive integer value, and you
  // can use x.toDouble to convert an x: Int to aPar Double.
  final def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (x, y) => (-x.toDouble / Int.MinValue, y)
  }


  // 6.3 Write functions to generate an (Int, Double) pair, aPar (Double, Int) pair, and aPar
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

  // 6.4 Write aPar function to generate aPar list of random integers.

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

  val intRand: Rand[Int] = _.nextInt

  final def unit[A](a: A): Rand[A] = rng => (a, rng)

  final def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  final def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)


  // 6.5 Use map to reimplement double in aPar more elegant way. See exercise 6.2.
  final def doubleMap: Rand[Double] = map(nonNegativeInt)(x => -x.toDouble / Int.MinValue)


  // 6.6  Write the implementation of map2 based on the following signature.
  // This function takes two actions, ra and bRand, and aPar function f for combining their results,
  // and returns aPar new action that combines them:
  final def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }


  final def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(intRand, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, intRand)


  // 6.7 Hard: If you can combine two RNG transitions, you should be able to combine aPar whole list of them.
  // Implement sequence for combining aPar List of transitions into aPar single transition.
  // Use it to reimplement the ints function you wrote before. For the latter, you can use the standard library
  // function List.fill(n)(x) to make aPar list with x repeated n times.

  //  We set sequence[A](Nil) = rng => (Nil,rng)
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]]
  = List.foldLeft[Rand[A], Rand[List[A]]](fs, rng => (List.Nil, rng))(ra => rla => map2(ra, rla)(List.Cons(_, _)))

  final def intsSequence(count: Int): Rand[List[Int]] = sequence[Int](List.fill[Rand[Int]](count)(intRand))

  final def nonNegativeLessThan(n: Int): Rand[Int] =  if (n<1) unit[Int](0) else rng => { val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        (mod, rng2)
      else nonNegativeLessThan(n)(rng)}

  // 6.8 Implement flatMap, and then use it to implement nonNegativeLessThan.
  final def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  final def zip[A, B](aRand: Rand[A])(bRand: Rand[B]): Rand[(A, B)] = rng => {
    val (a, rng2): (A, RNG) = aRand(rng)
    val (b, rng3): (B, RNG) = bRand(rng2)
    ((a, b), rng3)
  }

  final def nonNegativeLessThanFlatMap(n: Int): Rand[Int] = if (n<1) unit[Int](0)
  else flatMap[Int, Int](nonNegativeInt)(i => {
    val mod = i % n
    if (i + (n - 1) - mod >= 0) rng => (mod, rng) else rng => nonNegativeLessThanFlatMap(n)(rng)
  })

  // 6.9 Reimplement map and map2 in terms of flatMap. The fact that this is possible is what
  // we’re referring to when we say that flatMap is more powerful than map and map2.
  final def map2Flat[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    val abRand: Rand[(A, B)] = zip(ra)(rb)
    flatMap[(A, B), C](abRand)((ab => ab match {
      case (a, b)
      => rng => {
        val ((a2, b2), state2): ((A, B), RNG) = abRand(rng)
        (f(a2, b2), state2)
      }
    }))
  }

  final def sequenceFlat[A](fs: List[Rand[A]]): Rand[List[A]]
  = List.foldLeft[Rand[A], Rand[List[A]]](fs, rng => (List.Nil, rng))(ra => rla => map2Flat(ra, rla)(List.Cons(_, _)))

  final def intsSequenceFlat(count: Int): Rand[List[Int]] = sequenceFlat[Int](List.fill[Rand[Int]](count)(intRand))

  // I know that this is inefficient but it is beautiful.
  final def mapFlat[A, B](s: Rand[A])(f: A => B): Rand[B] = map2Flat[A, A, B](s, s)((a1, a2) => f(a1))

  final def doubleMapFlat: Rand[Double] = mapFlat(nonNegativeInt)(x => 0 - x.toDouble / Int.MinValue)


  // Chapter 6.5 A general state action data type

  //  EXERCISE 6.10
  //  Generalize the functions unit, map, map2, flatMap, and sequence. Add them as methods
  //  on the State case class where possible. Otherwise you should put them in aPar State
  //  companion object.

  case class State[S, +A](run: S => (A, S)) {
    final def flatMap[B](g: A => State[S, B]): State[S, B] = {
      // QUESTION: Why does IntelliJ complain about the type of the expression ??? The compiler does not !
      val res = State[S, B](state => {
        val (a, state2): (A, S) = run(state)
        g(a).run(state2)
      })
      res
    }


    final def zip[B](bState: State[S, B]): State[S, (A, B)]
    // QUESTION: Why does IntelliJ complain about the type of bRand.run(state)._1 ??? The compiler does not !
    //    = State[S, (A, B)](state => ((this.run(state)._1, bRand.run(state)._1), this.run(state)._2))
    = State[S, (A, B)](state => {
      val (a, state2): (A, S) = this.run(state)
      val (b, state3): (B, S) = bState.run(state2)
      ((a, b), state3)
    })

    // slow but beautiful using zip
    final def map2_slow[B, C](bState: State[S, B])(f: (A, B) => C): State[S, C]
    = {
      val abState: State[S, (A, B)] = this.zip(bState)
      abState.flatMap[C]((ab => ab match {
        case (a, b)
        => State[S, C](state => {
          //println("...map2: ab="+ab+"  f(aPar, b)="+f(aPar, b)+"  state="+state)
          val ((a2, b2), state2): ((A, B), S) = abState.run(state)
          (f(a2, b2), state2)
        })
      }))
    }

    final def map2[B, C](bState: State[S, B])(f: (A, B) => C): State[S, C] = {
      lazy val g:A => State[S, C] = a => State[S, C]( state => {
        val bRun : (B,S) = bState.run(state)
        (f(a,bRun._1),bRun._2)
      })
      this.flatMap[C](g)
    }

    final def map[B](f: A => B): State[S, B] = map2[A, B](this)((a1, a2) => f(a1))

  }

  type RandState[A] = State[RNG, A]


  final def intRandState: RandState[Int] = State[Ch06.RNG, Int](rng => rng.nextInt)

  final def nonNegativeIntRandState: RandState[Int] = State[Ch06.RNG, Int](nonNegativeInt)

  final def doubleRandState: RandState[Double] = State[Ch06.RNG, Double](doubleMap)

  final def nonNegativeLessThanState(n: Int): RandState[Int] = if (n<1) unitState[RNG, Int](0)
  else nonNegativeIntRandState.flatMap[Int](i => {
    val mod = i % n
    if (i + (n - 1) - mod >= 0) State[Ch06.RNG, Int](rng => (mod, rng))
    else this.nonNegativeLessThanState(n)
  })

  final def sequence[S, A](stateList: List[State[S, A]]): State[S, List[A]]
  = List.foldLeft[State[S, A], State[S, List[A]]](stateList, State[S, List[A]](state => (List.Nil, state)))(aState => aListState => aState.map2(aListState)(List.Cons(_, _)))

  final def intsSequenceState(count: Int): State[RNG, List[Int]] = sequence[RNG, Int](List.fill[RandState[Int]](count)(intRandState))

  final def unitState[S, A](a: A): State[S, A] = State[S, A](state => (a, state))

  final def doubleState: State[RNG, Double] = nonNegativeIntRandState.map(x => 0 - x.toDouble / Int.MinValue)


  //  6.6 Purely functional imperative programming
  //
  //  EXERCISE 6.11
  //  Hard: To gain experience with the use of State, implement aPar finite state automaton
  //  that models aPar simple candy dispenser. The machine has two types of input: you can
  //  insert aPar coin, or you can turn the knob to dispense candy. It can be in one of two
  //  states: locked or unlocked. It also tracks how many candies are left and how many
  //  coins it contains.
  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  final def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def processInput(inp: List[Input])(mac: Machine): ((Int, Int), Machine) = {
      val doNothingResult: ((Int, Int), Machine) = ((mac.candies, mac.coins), mac)
      if (mac.candies < 1) doNothingResult
      else {
        inp match {
          case List.Cons(inp, inpList) => if (mac.locked == (inp == Turn)) processInput(inpList)(mac)
          else if (inp == Turn) processInput(inpList)(Machine(!mac.locked, mac.candies - 1, mac.coins)) else processInput(inpList)(Machine(!mac.locked, mac.candies, mac.coins + 1))
          case _ => doNothingResult
        }
      }
    }
    State[Machine, (Int, Int)](processInput(inputs))
  }

}


object nith_Chapter_06 extends App {

  val rng0: Ch06.RNG = Ch06.SimpleRNG(0)
  val simpleRNGiterator: Int => Ch05.Stream[(Int, Ch06.RNG)] = count => Ch05.unfold2[(Int, Ch06.RNG), Ch06.RNG](count)(rng0)(rng => {
    val (n, rng2) = rng.nextInt
    ((n, rng2), rng2)
  })

  val SimpleRNGstream: (Long => Ch05.Stream[Int]) = fstSeed => Ch05.unfold[Int, Ch06.RNG](Ch06.SimpleRNG(fstSeed))(rng => Some(rng.nextInt))

  util.log("****** Chapter_06 ******")
  println("\n* Some experssions using Int.MaxValue and Int.MinValue *")
  util.log("Int.MinValue = %s".format(Int.MinValue))
  util.log("Int.MaxValue =  %s".format(Int.MaxValue))
  util.log("Long.MinValue = %s".format(Long.MinValue))
  util.log("Long.MaxValue =  %s".format(Long.MaxValue))
  util.log("rng0.nextInt = %s".format(rng0.nextInt))
  util.log("-1-Int.MinValue = %s".format(-1 - Int.MinValue))
  util.log("Int.MinValue-Int.MinValue = %s".format(Int.MinValue - Int.MinValue))
  util.log("(-1)*Int.MaxValue-Int.MinValue = %s".format((-1) * Int.MaxValue - Int.MinValue))
  util.log("(Int.MinValue/2)-(Int.MinValue/2) = %s".format((Int.MinValue / 2) - Int.MinValue / 2))
  util.log("(Int.MaxValue/2)-(Int.MinValue/2) = %s".format((Int.MaxValue / 2) - Int.MinValue / 2))
  println("\nsimpleRNGiterator(8) = %s".format(simpleRNGiterator(8).myString))
  util.log("SimpleRNG(Long.MinValue).nextInt = %s".format(Ch06.SimpleRNG(Long.MinValue).nextInt))
  util.log("SimpleRNG(-1).nextInt = %s".format(Ch06.SimpleRNG(-1).nextInt))
  util.log("SimpleRNG(Long.MaxValue).nextInt = %s".format(Ch06.SimpleRNG(Long.MaxValue).nextInt))
  util.log("SimpleRNGstream(Long.MinValue).take(10) = %s".format(SimpleRNGstream(Long.MinValue).take(10).myString))
  util.log("SimpleRNGstream(0).take(20) = %s".format(SimpleRNGstream(0).take(20).myString))
  util.log("SimpleRNGstream(42).take(20) = %s".format(SimpleRNGstream(42).take(20).myString))

  println("\n** Exercise 6.1 **")
  util.log("unfold(rng0)(rng => Some(nonNegativeInt(rng))).take(20)\n  = %s"
    .format(Ch05.unfold[Int, Ch06.RNG](rng0)(rng => Some(Ch06.nonNegativeInt(rng))).take(20).myString))

  println("\n** Exercise 6.2 **")
  util.log("unfold(rng0)(rng => Some(double(rng))).take(20)\n  = %s"
    .format(Ch05.unfold[Double, Ch06.RNG](rng0)(rng => Some(Ch06.double(rng))).take(20).myString))

  println("\n** Exercise 6.3 **")
  util.log("unfold(SimpleRNG(-1))(rng => Some(intDouble(rng))).take(10)\n  = %s"
    .format(Ch05.unfold[(Int, Double), Ch06.RNG](Ch06.SimpleRNG(-1))(rng => Some(Ch06.intDouble(rng))).take(10).myString))
  util.log("unfold(SimpleRNG(-1))(rng => Some(doubleInt(rng))).take(10)\n  = %s"
    .format(Ch05.unfold[(Double, Int), Ch06.RNG](Ch06.SimpleRNG(-1))(rng => Some(Ch06.doubleInt(rng))).take(10).myString))
  util.log("unfold(SimpleRNG(-1))(rng => Some(double3(rng))).take(10)\n  = %s"
    .format(Ch05.unfold[(Double, Double, Double), Ch06.RNG](Ch06.SimpleRNG(-1))(rng => Some(Ch06.double3(rng))).take(10).myString))

  println("\n** Exercise 6.4 **")
  util.log("ints(-1)(rng0) = %s".format(Ch06.ints(-1)(rng0)))
  util.log("ints(0)(rng0) = %s".format(Ch06.ints(0)(rng0)))
  util.log("ints(1)(rng0) = %s".format(Ch06.ints(1)(rng0)))
  util.log("ints(3)(rng0) = %s".format(Ch06.ints(3)(rng0)))
  util.log("ints(10)(rng0) = %s".format(Ch06.ints(10)(rng0)))

  println("\n** Exercise 6.5 **")
  util.log("unfold(rng0)(rng => Some(double(rng))).take(20)\n  = %s"
    .format(Ch05.unfold[Double, Ch06.RNG](rng0)(rng => Some(Ch06.double(rng))).take(10).myString))
  util.log("unfold2(rng0)(3)(doubleMap)\n  = %s"
    .format(Ch05.unfold2[Double, Ch06.RNG](10)(rng0)(Ch06.doubleMap).myString))

  println("\n** Exercise 6.7 **")
  util.log("intsSequence(-1)(rng0) = %s".format(Ch06.intsSequence(-1)(rng0)))
  util.log("intsSequence(0)(rng0)  = %s".format(Ch06.intsSequence(0)(rng0)))
  util.log("intsSequence(1)(rng0)  = %s".format(Ch06.intsSequence(1)(rng0)))
  util.log("intsSequence(3)(rng0)  = %s".format(Ch06.intsSequence(3)(rng0)))
  util.log("intsSequence(10)(rng0) = %s".format(Ch06.intsSequence(10)(rng0)))
  util.log("intsSequence(25)(rng0) = %s".format(Ch06.intsSequence(25)(rng0)))

  println("\n** Exercise 6.8 **")
  util.log("unfold2(20)(rng0)(nonNegativeLessThan(20)        = %s"
    .format(Ch05.unfold2[Int, Ch06.RNG](20)(rng0)(Ch06.nonNegativeLessThan(20)).myString))
  util.log("unfold2(20)(rng0)(nonNegativeLessThanFlatMap(20) = %s"
    .format(Ch05.unfold2[Int, Ch06.RNG](20)(rng0)(Ch06.nonNegativeLessThanFlatMap(20)).myString))

  println("\n** Exercise 6.9 **")
  util.log("intsSequenceFlat(10)(rng0) = %s".format(Ch06.intsSequenceFlat(10)(rng0)))

  util.log("** Exercise 6.10 **")
  util.log("unfold2(10)(rng0)(rng => nonNegativeInt(rng))          = %s"
    .format(Ch05.unfold2[Int, Ch06.RNG](10)(rng0)(rng => Ch06.nonNegativeInt(rng)).myString))
  util.log("unfold2(10)(rng0)(rng => nonNegativeIntRandState.run(rng)) = %s"
    .format(Ch05.unfold2[Int, Ch06.RNG](10)(rng0)(rng => Ch06.nonNegativeIntRandState.run(rng)).myString))
  util.log("unfold2(10)(rng0)(rng => double(rng))          = %s"
    .format(Ch05.unfold2[Double, Ch06.RNG](10)(rng0)(rng => Ch06.double(rng)).myString))
  util.log("unfold2(10)(rng0)(rng => doubleRandState.run(rng)) = %s"
    .format(Ch05.unfold2[Double, Ch06.RNG](10)(rng0)(rng => Ch06.doubleRandState.run(rng)).myString))
  util.log("unfold2(20)(rng0)(nonNegativeLessThan(20)                       = %s"
    .format(Ch05.unfold2[Int, Ch06.RNG](20)(rng0)(Ch06.nonNegativeLessThan(20)).myString))
  util.log("unfold2(20)(rng0)(rng => nonNegativeLessThanState(20).run(rng)) = %s"
    .format(Ch05.unfold2[Int, Ch06.RNG](20)(rng0)(rng => Ch06.nonNegativeLessThanState(20).run(rng)).myString))
  util.log("unfold2(10)(rng0)(rng => nonNegativeIntRandState.zip(doubleRandState).run(rng))) = %s"
    .format(Ch05.unfold2[(Int, Double), Ch06.RNG](10)(rng0)(rng => Ch06.nonNegativeIntRandState.zip(Ch06.doubleRandState).run(rng)).myString))



  println("\nsimpleRNGiterator(16) = %s".format(simpleRNGiterator(16).myString))
  println("\nintsSequenceFlat(0)(rng0)       = %s".format(Ch06.intsSequenceFlat(0)(rng0)))
  util.log("intsSequenceState(0).run(rng0)  = %s".format(Ch06.intsSequenceState(0).run(rng0)))
  println("\nintsSequenceFlat(1)(rng0)       = %s".format(Ch06.intsSequenceFlat(1)(rng0)))
  util.log("intsSequenceState(1).run(rng0)  = %s".format(Ch06.intsSequenceState(1).run(rng0)))
  println("\nintsSequenceFlat(2)(rng0)       = %s".format(Ch06.intsSequenceFlat(2)(rng0)))
  util.log("intsSequenceState(2).run(rng0)  = %s".format(Ch06.intsSequenceState(2).run(rng0)))
  println("\nintsSequenceFlat(3)(rng0)       = %s".format(Ch06.intsSequenceFlat(3)(rng0)))
  util.log("intsSequenceState(3).run(rng0)  = %s".format(Ch06.intsSequenceState(3).run(rng0)))
  println("\nintsSequenceFlat(4)(rng0)       = %s".format(Ch06.intsSequenceFlat(4)(rng0)))
  util.log("intsSequenceState(4).run(rng0)  = %s".format(Ch06.intsSequenceState(4).run(rng0)))
  util.log("intsSequenceFlat(10)(rng0)      = %s".format(Ch06.intsSequenceFlat(10)(rng0)))
  util.log("intsSequenceState(10).run(rng0) = %s".format(Ch06.intsSequenceState(10).run(rng0)))

  println("\nunfold2(rng0)(3)(doubleMap) = %s"
    .format(Ch05.unfold2[Double, Ch06.RNG](16)(rng0)(Ch06.doubleMap).myString))
  util.log("unfold2(rng0)(3)(doubleState) = %s"
    .format(Ch05.unfold2[Double, Ch06.RNG](8)(rng0)(rng => Ch06.doubleState.run(rng)).myString))

  println("\n** Exercise 6.11 **")
  util.log("simulateMachine().run(Machine(true,1,1)) = %s".format(Ch06.simulateMachine(List.Nil).run(Ch06.Machine(true, 1, 1))))
  util.log("simulateMachine(Coin,Turn,Turn,Turn,Turn,Coin,Turn).run(Machine(true,0,42)) = %s"
    .format(Ch06.simulateMachine(List[Ch06.Input](Ch06.Coin, Ch06.Turn, Ch06.Turn, Ch06.Turn, Ch06.Turn, Ch06.Coin, Ch06.Turn)).run(Ch06.Machine(true, 0, 42))))
  util.log("simulateMachine(Coin).run(Machine(false,24,42)) = %s".format(Ch06.simulateMachine(List[Ch06.Input](Ch06.Coin)).run(Ch06.Machine(false, 24, 42))))
  util.log("simulateMachine(Turn).run(Machine(true,24,42)) = %s".format(Ch06.simulateMachine(List[Ch06.Input](Ch06.Turn)).run(Ch06.Machine(true, 24, 42))))
  util.log("simulateMachine(Coin,Turn).run(Machine(true,3,42)) = %s"
    .format(Ch06.simulateMachine(List[Ch06.Input](Ch06.Coin, Ch06.Turn)).run(Ch06.Machine(true, 3, 42))))
  util.log("simulateMachine(Coin,Turn,Turn,Turn,Turn,Coin,Turn).run(Machine(true,24,42)) = %s"
    .format(Ch06.simulateMachine(List[Ch06.Input](Ch06.Coin, Ch06.Turn, Ch06.Turn, Ch06.Turn, Ch06.Turn, Ch06.Coin, Ch06.Turn)).run(Ch06.Machine(true, 24, 42))))
  util.log("simulateMachine(Turn,Coin,Turn,Turn,Coin,Turn,Turn,Coin,Turn).run(Machine(true,24,42)) = %s"
    .format(Ch06.simulateMachine(List[Ch06.Input](Ch06.Turn, Ch06.Coin, Ch06.Turn, Ch06.Turn, Ch06.Coin, Ch06.Turn, Ch06.Turn, Ch06.Coin, Ch06.Turn)).run(Ch06.Machine(true, 24, 42))))
  util.log("simulateMachine(Coin,Coin,Coin,Coin,Coin,Coin,Coin,Coin,Coin).run(Machine(true,24,42)) = %s"
    .format(Ch06.simulateMachine(List[Ch06.Input](Ch06.Coin, Ch06.Coin, Ch06.Coin, Ch06.Coin, Ch06.Coin, Ch06.Coin, Ch06.Coin, Ch06.Coin, Ch06.Coin)).run(Ch06.Machine(true, 24, 42))))
  util.log("simulateMachine(Turn,Turn,Turn,Turn,Turn,Turn,Turn,Turn,Turn).run(Machine(true,24,42)) = %s"
    .format(Ch06.simulateMachine(List[Ch06.Input](Ch06.Turn, Ch06.Turn, Ch06.Turn, Ch06.Turn, Ch06.Turn, Ch06.Turn, Ch06.Turn, Ch06.Turn, Ch06.Turn)).run(Ch06.Machine(true, 24, 42))))
  util.log("*** Chapter 06 Done ***")

}
