import util.log
import List._
import Ch04_Option.{Option, Some}
import Ch05.{Stream, unfold, unfold2}
import Ch06._
import Ch07.Phase3._

object Ch08 {

  // 8.1 To get used to thinking about testing in this way, come up with properties that specify the implementation
  // of a sum: List[Int] => Int function. You don’t have to write your properties down as executable ScalaCheck
  // code an informal description is fine.

  // 8.2 What properties specify a function that finds the maximum of a List[Int]?

  object Phase1 {

    // 8.3 Assuming the following representation of Prop, implement && as a method of Prop.
    trait Prop {
      def check: Boolean

      def &&(p: Prop): Prop = new Prop {
        def check: Boolean = this.check && p.check
      }
    }

  }

  object Phase2 {
    type FailedCase = String
    type SuccessCount = Int

    /*    trait Prop {
          def check: Either[(FailedCase, SuccessCount), SuccessCount]
        }*/

    case class Gen[+A](sample: State[RNG, A]) {
      // 8.6 Implement flatMap, and then use it to implement this more dynamic version of listOfN.
      def flatMap[B](f: A => Gen[B]): Gen[B] = new Gen[B](this.sample.flatMap[B](f(_).sample))

      def listOfN(size: Gen[Int]): Gen[List[A]] = {
        val runOfAs: RNG => (List[A], RNG) = rng => sequence[RNG, A](List.fill[RandState[A]](size.sample.run(rng)._1)(this.sample)).run(rng)
        val stateOfAs: State[RNG, List[A]] = new State[RNG, List[A]](runOfAs)
        new Gen[List[A]](stateOfAs)
      }

      // 8.10 Implement helper functions for converting Gen to SGen. You can add this as a method on Gen.
      def unsized: SGen[A] = new SGen[A](n => this)
    }

    // 8.4 Implement Gen.choose using this representation of Gen. It should generate integers in the range
    // start to stopExclusive. Feel free to use functions you’ve already written.
    final def intGen: Gen[Int] = new Gen[Int](intRandState)

    final def choose(start: Int, stopExclusive: Int): Gen[Int] = new Gen[Int](nonNegativeLessThanState(stopExclusive - start).map[Int](_ + start))

    final def choose(start: Double, stopExclusive: Double): Gen[Double] = new Gen[Double](doubleRandState.map[Double](x => (stopExclusive - start) * x + start))

    // 8.5 Let’s see what else we can implement using this representation of Gen.
    // Try implementing unit, boolean, and listOfN.
    final def unit[A](a: => A): Gen[A] = new Gen[A](unitState[RNG, A](a))

    final def boolean: Gen[Boolean] = new Gen[Boolean](new State[RNG, Boolean](rng => rng.nextInt match {
      case (n, r) => (n % 2 == 0, r)
    }))

    final def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      val stateOfAs: State[RNG, List[A]] = sequence[RNG, A](List.fill[RandState[A]](n)(g.sample))
      new Gen[List[A]](stateOfAs)
    }

    // 8.7 Implement union, for combining two generators of the same type into one,
    // by pulling values from each generator with equal likelihood.
    final def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap[A](p => if (p) g1 else g2)

    final def unionLazy[A](g1: => Gen[A], g2: => Gen[A]): Gen[A] = boolean.flatMap[A](p => if (p) g1 else g2)

    // 8.8 Implement weighted, a version of union that accepts a weight for each Gen and generates values
    // from each Gen with probability proportional to its weight.
    final def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = choose(0.0 - g1._2, g2._2).flatMap[A](x => if (x < 0) g1._1 else g2._1)

    final def weighted2[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val g1weight: Double = g1._2 / (g1._2 + g2._2)
      val doubleGen: Gen[Double] = new Gen[Double](doubleRandState)
      log("...weighted : g1weight=" + g1weight)
      doubleGen.flatMap[A]((x => if (x < g1weight) g1._1 else g2._1))
    }

    type TestCases = Int

    sealed trait Result {
      def isFalsified: Boolean
    }

    case object Passed extends Result {
      final def isFalsified = false
    }

    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
      final def isFalsified = true

      //      final def falsifiedString : String = "(failure="+failure+",successes="+successes+")"
    }

    case class Prop(run: (TestCases, RNG) => Result) {
      // EXERCISE 8.9
      // Now that we have a representation of Prop, implement && and || for composing Prop values. Notice that in the
      // case of failure we don’t know which property was responsible, the left or the right. Can you devise a way of
      // handling this, perhaps by allowing Prop values to be assigned a tag or label which gets displayed in the event
      // of a failure?
      final def &&(p: Prop): Prop = {
        def newRunner(tc: TestCases, r: RNG): Result = this.run(tc, r) match {
          case Passed => p.run(tc, r) match {
            case Falsified(rFail, rSuc) => new Falsified("Prop.&&: LEFT prop passed but RIGHT Prop falsified: FailedCase=" + rFail + " SuccessCount=" + rSuc, rSuc)
            case Passed => Passed
          }
          case Falsified(lFail, lSuc) => p.run(tc, r) match {
            case Passed => new Falsified("Prop.&&: RIGHT Prop passed but LEFT Prop falsified: FailedCase=" + lFail + " SuccessCount=" + lSuc, lSuc)
            case Falsified(rFail, rSuc) => new Falsified("Prop.&&: BOTH Props falsified: (LEFT FailedCase=" + lFail + " SuccessCount=" + lSuc + ", RIGHT FailedCase=" + rFail + " SuccessCount=" + rSuc + ")", lSuc.min(rSuc))
          }
        }
        Prop(newRunner)
      }

      final def ||(p: Prop): Prop = {
        def newRunner(tc: TestCases, r: RNG): Result = this.run(tc, r) match {
          case Passed => Passed
          case Falsified(lFail, lSuc) => p.run(tc, r) match {
            case Passed => Passed
            case Falsified(rFail, rSuc) => new Falsified("Prop.&&: BOTH Props falsified: (LEFT FailedCase=" + lFail + " SuccessCount=" + lSuc + ", RIGHT FailedCase=" + rFail + " SuccessCount=" + rSuc + ")", lSuc.max(rSuc))
          }
        }
        Prop(newRunner)
      }
    }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = unfold(rng)(rng => Some(g.sample.run(rng)))

    def randomList[A](g: Gen[A])(rng: RNG)(size: Int): List[A] = randomStream[A](g)(rng).take(size).toList

    def randomIntList(size: Int): List[Int] = randomList[Int](intGen)(SimpleRNG(System.currentTimeMillis))(size)

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    def forAll[A](aGen: Gen[A])(P: A => Boolean): Prop = Prop {
      (n, rng) => {
        val lazyIdentiy: (=> Int) => Int = n => n
        val indexedAstream: Stream[(A, Int)] = randomStream[A](aGen)(rng).zip[Int](Stream(lazyIdentiy).take(n))
        val mapli: (=> Tuple2[A, Int]) => Result = x => try {
          lazy val testResult: Boolean = P(x._1)
          if (testResult) Passed else Falsified(x._1.toString, x._2)
        } catch {
          case e: Exception => Falsified(buildMsg(x._1, e), x._2)
        }
        val resultOption: Option[Result] = indexedAstream.map[Result](mapli).find(_.isFalsified)
        resultOption.getOrElse(Passed)
      }
    }

    case class SGen[+A](forSize: Int => Gen[A]) {
      // 8.11 Not surprisingly, SGen at a minimum supports many of the same operations as Gen, and the implementations
      // are rather mechanical. Define some convenience functions on SGen that simply delegate to the corresponding
      // functions on Gen.
      def flatMap[B](f: A => SGen[B]): SGen[B] = new SGen[B](n => this.forSize(n).flatMap[B](a => f(a).forSize(n)))

      def listOfN(size: SGen[Int]): SGen[List[A]] = new SGen[List[A]](n => this.forSize(n).listOfN(size.forSize(n)))

    }

    // 8.12 Implement a listOf combinator that doesn’t accept an explicit size. It should return an SGen instead of a
    // Gen. The implementation should generate lists of the requested size.
    def listOf[A](g: Gen[A]): SGen[List[A]] = new SGen[List[A]](n => g.listOfN(unit(n)))

  }

  object Phase3 {

    import Phase2.{Gen, Passed, Falsified, Result, SGen, TestCases, unit}

    type MaxSize = Int

    case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
      // EXERCISE 8.9
      // Now that we have a representation of Prop, implement && and || for composing Prop values. Notice that in the
      // case of failure we don’t know which property was responsible, the left or the right. Can you devise a way of
      // handling this, perhaps by allowing Prop values to be assigned a tag or label which gets displayed in the event
      // of a failure?
      final def &&(p: Prop): Prop = {
        def newRunner(ms: MaxSize, tc: TestCases, r: RNG): Result = this.run(ms, tc, r) match {
          case Passed => p.run(ms, tc, r) match {
            case Falsified(rFail, rSuc) => new Falsified("Prop.&&: LEFT prop passed but RIGHT Prop falsified: FailedCase=[" + rFail + "] SuccessCount=" + rSuc, rSuc)
            case Passed => Passed
          }
          case Falsified(lFail, lSuc) => p.run(ms, tc, r) match {
            case Passed => new Falsified("Prop.&&: RIGHT Prop passed but LEFT Prop falsified: FailedCase=[" + lFail + "] SuccessCount=" + lSuc, lSuc)
            case Falsified(rFail, rSuc) => new Falsified("Prop.&&: BOTH Props falsified: (LEFT FailedCase=[" + lFail + "] SuccessCount=" + lSuc + ", RIGHT FailedCase=[" + rFail + "] SuccessCount=" + rSuc + ")", lSuc.min(rSuc))
          }
        }
        Prop(newRunner)
      }

      final def ||(p: Prop): Prop = {
        def newRunner(ms: MaxSize, tc: TestCases, r: RNG): Result = this.run(ms, tc, r) match {
          case Passed => Passed
          case Falsified(lFail, lSuc) => p.run(ms, tc, r) match {
            case Passed => Passed
            case Falsified(rFail, rSuc) => new Falsified("Prop.&&: BOTH Props falsified: (LEFT FailedCase=[" + lFail + "] SuccessCount=" + lSuc + ", RIGHT FailedCase=[" + rFail + "] SuccessCount=" + rSuc + ")", lSuc.max(rSuc))
          }
        }
        Prop(newRunner)
      }

      final def toBool(ms: MaxSize)(tc: TestCases)(r: RNG): Boolean = !this.run(ms, tc, r).isFalsified

    }

    // 2 usefull constants for the type Prop
    def alwaysFalsified: Prop = new Prop((_, _, _) => Falsified("always Falsified", 0))

    def alwaysPassed: Prop = new Prop((_, _, _) => Passed)

    def run(
             p: Prop,
             maxSize: Int = 128,
             testCases: Int = 3 * 128,
             rng: RNG = SimpleRNG(System.currentTimeMillis)
             ): String =
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) => "Falsified after " + n + " passed tests:\n" + msg
        case Passed => "Passed " + testCases + " tests."
      }

    //    def forAll[A](g: SGen[A])(P: A => Boolean): Prop = forAll(g.forSize)(P)

    def forAll[A](g: Int => Phase2.Gen[A])(P: A => Boolean): Prop = Prop {
      (max, numTestCases, rng) =>
        val casesPerSize: Int = (numTestCases + max) / (max.abs + 1)
        val propSequence: Int => Phase2.Prop = i => Phase2.forAll(g(i))(P)
        val propPhase2List: List[Phase2.Prop] = List.map(List.integers(0)(numTestCases.min(max)))(propSequence)
        val propPhase3List: List[Prop] = List.map(propPhase2List)(p => Prop {
          (_, _, rng) => p.run(casesPerSize, rng)
        })
        val prop: Prop = List.foldLeft[Prop, Prop](propPhase3List, alwaysPassed)(prop1 => prop2 => prop2.&&(prop1))
        //        log("...Phase3.forAll: max="+max+"  numTestCases="+numTestCases+"    casesPerSize="+casesPerSize+"   propPhase2List.size="+List.length(propPhase2List)+"   propPhase3List.size="+List.length(propPhase3List))
        prop.run(max, numTestCases, rng)
    }

    def forAllAll[A](g: Int => Phase2.Gen[A])(P: A => Prop): Prop = Prop {
      (max, numTestCases, rng) =>
        val casesPerSize: Int = (numTestCases + max) / (max.abs + 1)
        val propSequence: Int => Phase2.Prop = i => Phase2.forAll(g(i))(a => P(a).toBool(max)(numTestCases)(rng))
        val propPhase2List: List[Phase2.Prop] = List.map(List.integers(0)(numTestCases.min(max)))(propSequence)
        val propPhase3List: List[Prop] = List.map(propPhase2List)(p => Prop {
          (_, _, rng) => p.run(casesPerSize, rng)
        })
        val prop: Prop = List.foldLeft[Prop, Prop](propPhase3List, alwaysPassed)(prop1 => prop2 => prop2.&&(prop1))
//        log("...Phase3.forAllAll: max=" + max + "  numTestCases=" + numTestCases + "    casesPerSize=" + casesPerSize + "   propPhase2List.size=" + List.length(propPhase2List) + "   propPhase3List.size=" + List.length(propPhase3List))
        prop.run(max, numTestCases, rng)
    }

  }

}

object nith_Chapter_08 extends App {

  import Ch08.Phase2.{boolean, choose, forAll, Gen, intGen, listOf, listOfN, Passed, randomIntList, Result, SGen, union, unit, weighted}
  import Ch08.Phase3.{Prop, run}
  import java.util.concurrent._
  import java.util.concurrent.atomic.AtomicInteger

  println("****** Chapter_08 ******")
  val rng0: RNG = SimpleRNG(0)
  val rng1: RNG = SimpleRNG(0).nextInt._2
  val simpleRNGiterator: Int => Stream[(Int, RNG)] = count => unfold2[(Int, RNG), RNG](count)(rng0)(rng => {
    val (n, rng2) = rng.nextInt
    ((n, rng2), rng2)
  })
  val esUnlimited: ExecutorService = Executors.newCachedThreadPool(new ThreadFactory {
    val counter = new AtomicInteger(0)

    def newThread(r: Runnable): Thread = {
      val t = new Thread(r, s"PAR-thread-${counter.getAndIncrement}")
      t.setDaemon(true)
      t
    }
  })

  log("rng0 = %s".format(rng0))
  log("rng1 = %s".format(rng1))
  log("simpleRNGiterator(9) = %s".format(simpleRNGiterator(9).myString))

  println("\n** Exercise 8.4 **")
  log("choose(0,42).sample.run(rng0)                                  = %s".format(choose(0, 42).sample.run(rng0)))
  log("choose(0,42).sample.run(choose(0,42).sample.run(rng0)._2))     = %s".format(choose(0, 42).sample.run(choose(0, 42).sample.run(rng0)._2)))
  log("choose(0.0,42.0).sample.run(rng0)                              = %s".format(choose(0.0, 42.0).sample.run(rng0)))
  log("choose(0.0,42.0).sample.run(choose(0,42).sample.run(rng0)._2)) = %s".format(choose(0.0, 42.0).sample.run(choose(0.0, 42.0).sample.run(rng0)._2)))

  println("\n** Exercise 8.5 **")
  log("unit(\"a\").sample.run(rng0)                          = %s".format(unit("a").sample.run(rng0)))
  log("unit(\"a\").sample.run(unit(\"a\").sample.run(rng0)._2) = %s".format(unit("a").sample.run(unit("a").sample.run(rng0)._2)))
  log("boolean.sample.run(rng0)                            = %s".format(boolean.sample.run(rng0)))
  log("boolean.sample.run(boolean.sample.run(rng0))        = %s".format(boolean.sample.run(boolean.sample.run(rng0)._2)))
  log("listOfN(10,unit(42)).sample.run(rng0)               = %s".format(listOfN(10, unit(42)).sample.run(rng0)))
  log("listOfN[Int](10,choose(0,42)).sample.run(rng0)      = %s".format(listOfN[Int](10, choose(0, 42)).sample.run(rng0)))

  println("\n** Exercise 8.6 **")
  log("unit(\"a\").listOfN(unit(10)).sample.run(rng0)        = %s".format(unit("a").listOfN(unit(10)).sample.run(rng0)))
  log("choose(0,42).listOfN(unit(10)).sample.run(rng0)     = %s".format(choose(0, 42).listOfN(unit(10)).sample.run(rng0)))
  log("intGen.listOfN(unit(10)).sample.run(rng0)           = %s".format(intGen.listOfN(unit(10)).sample.run(rng0)))

  println("\n** Exercise 8.7 and 8.8**")
  log("union(choose(-42,-1),choose(1,42)).listOfN(unit(20)).sample.run(rng0))               = %s".format(union(choose(-42, -1), choose(1, 42)).listOfN(unit(20)).sample.run(rng0)))
  log("weighted((choose(-42,-1),0.0),(choose(1,42),1.0)).listOfN(unit(20)).sample.run(rng0) = %s".format(weighted((choose(-42, -1), 0.0), (choose(1, 42), 1.0)).listOfN(unit(20)).sample.run(rng0)))
  log("weighted((choose(-42,-1),0.1),(choose(1,42),0.9)).listOfN(unit(20)).sample.run(rng0) = %s".format(weighted((choose(-42, -1), 0.1), (choose(1, 42), 0.9)).listOfN(unit(20)).sample.run(rng0)))
  log("weighted((choose(-42,-1),0.2),(choose(1,42),0.8)).listOfN(unit(20)).sample.run(rng0) = %s".format(weighted((choose(-42, -1), 0.2), (choose(1, 42), 0.8)).listOfN(unit(20)).sample.run(rng0)))
  log("weighted((choose(-42,-1),0.5),(choose(1,42),0.5)).listOfN(unit(20)).sample.run(rng0) = %s".format(weighted((choose(-42, -1), 0.5), (choose(1, 42), 0.5)).listOfN(unit(20)).sample.run(rng0)))

  println("\n** Exercise 8.9 **")
  log("choose(1,100).listOfN(unit(10)).sample.run(rng0)      = %s".format(choose(1, 100).listOfN(unit(10)).sample.run(rng0)))
  log("choose(1,100))(n => n MOD 5 > 0).run(10,rng0)         = %s".format(forAll[Int](choose(1, 100))(n => n % 5 > 0).run(10, rng0)))
  log("choose(1,100))(n => n MOD 7 > 0).run(10,rng0)         = %s".format(forAll[Int](choose(1, 100))(n => n % 7 > 0).run(10, rng0)))
  log("conjunction &&                                        = %s".format(forAll[Int](choose(1, 100))(n => n % 5 > 0).&&(forAll[Int](choose(1, 100))(n => n % 7 > 0)).run(10, rng0)))
  log("disjunction ||                                        = %s".format(forAll[Int](choose(1, 100))(n => n % 5 > 0).||(forAll[Int](choose(1, 100))(n => n % 7 > 0)).run(10, rng0)))
  log("choose(1,100))(n => n MOD 5 > 0).run(10,rng0) && true = %s".format(forAll[Int](choose(1, 100))(n => n % 5 > 0).&&(forAll[Int](unit(0))(_ => true)).run(10, rng0)))
  log("choose(1,100))(n => n MOD 5 > 0).run(10,rng0) || true = %s".format(forAll[Int](choose(1, 100))(n => n % 5 > 0).||(forAll[Int](unit(0))(_ => true)).run(10, rng0)))

  println("\n** Exercise 8.12 **")
  log("listOf(unit(\"a\")).forSize(0).sample.run(rng0)    = %s".format(listOf(unit("a")).forSize(0).sample.run(rng0)))
  log("listOf(choose(0,42)).forSize(0).sample.run(rng0) = %s".format(listOf(choose(0, 42)).forSize(0).sample.run(rng0)))
  log("listOf(unit(\"a\")).forSize(4).sample.run(rng0)    = %s".format(listOf(unit("a")).forSize(4).sample.run(rng0)))
  log("listOf(choose(0,42)).forSize(4).sample.run(rng0) = %s".format(listOf(choose(0, 42)).forSize(4).sample.run(rng0)))
  log("listOf(unit(\"a\")).forSize(8).sample.run(rng0)    = %s".format(listOf(unit("a")).forSize(8).sample.run(rng0)))
  log("listOf(choose(0,42)).forSize(8).sample.run(rng0) = %s".format(listOf(choose(0, 42)).forSize(8).sample.run(rng0)))

  println("\n** Exercise 8.13 **")
  // a few more generators
  val intListGen: Int => Gen[List[Int]] = n => listOfN(n, intGen)
  val maxIsTheBiggest: List[Int] => Boolean = (ns: List[Int]) => {
    val max: Int = List.max(ns)
    log("...maxIsTheBiggest: testing !List.exists(" + List.myString(ns) + ")(_ > max)=" + (!List.exists(ns)(_ > max)))
    !List.exists(ns)(_ > max)
  }
  val maxProp: Prop = Ch08.Phase3.forAll[List[Int]](intListGen)(maxIsTheBiggest)
  log("maxIsTheBiggest(List.Nil) = " + maxIsTheBiggest(List.Nil))
  log("run(maxProp) = " + run(maxProp, 5, 20))

  println("\n** Exercise 8.14 **")
  println("** Write a property to verify the behavior of List.sorted ...")
  println("* Testing predicate List.isSorted for Nil *")
  log("List.isSorted(List.Nil) = " + List.isSorted(List.Nil))
  println("\n* Testing predicate List.isSorted for thre recursive built-up of lists using forAllAll *\n")
  println("* A few cases with logging *")
  val loggedTest1: Boolean => Int => List[Int] => Boolean = debug => n => l => {
    val result: Boolean = List.isSorted(Cons(n, l)) == ( n <= List.min(l) && List.isSorted(l) )
    if (debug) log("... test function with \tl=" + List.myString(l) + "\tn=" + n + "\tList.min(l)=" + List.min(l)
      + "\tList.isSorted(l)=" + List.isSorted(l) + "\tn <= List.min(l)=" + (n <= List.min(l))
      + ":\tList.isSorted(" + List.myString(Cons(n, l)) + ")=" + List.isSorted(Cons(n, l)) + ":\t" + result)
    result
  }
  log("run(forAllAll[Int](_=>intGen)(n=> forAll[List[Int]](intListGen)(List.isSorted(Cons(n, l)) == (n <= List.min(l) && List.isSorted(l)))),2,4) = "
    + run(Ch08.Phase3.forAllAll[Int](_ => intGen)(n => Ch08.Phase3.forAll[List[Int]](intListGen)(loggedTest1(true)(n))), 2, 6))

  println("* Many cases without logging *")
  log("run(forAllAll[Int](_=>intGen)(n=> forAll[List[Int]](intListGen)(List.isSorted(Cons(n, l)) == (n <= List.min(l) && List.isSorted(l)))),2,4) = "
    + run(Ch08.Phase3.forAllAll[Int](_ => intGen)(n => Ch08.Phase3.forAll[List[Int]](intListGen)(loggedTest1(false)(n))), 10, 100))

  println("* Different test for isSorted using 2 forAllAll *")
  val loggedTest2: Boolean => Int => Int => List[Int] => Boolean = debug => n => m => l => {
    val result: Boolean = List.isSorted(Cons(n,(Cons(m, l)))) == (n <= m && List.isSorted(Cons(m, l)) )
    if (debug) log("... test function with \tl=" + List.myString(l) + "\tn=" + n  + "\tm=" + m
      + "\tList.isSorted(Cons(m, l))=" + List.isSorted(Cons(m, l)) + "\tn <= m=" + (n <= m)
      + ":\tList.isSorted(" + List.myString(Cons(n,(Cons(m, l)))) + ")=" + List.isSorted(Cons(n,(Cons(m, l)))) + ":\t" + result)
    result
  }
  log("XXX = "
    + run(Ch08.Phase3.forAllAll[Int](_ => intGen)(i =>
    Ch08.Phase3.forAllAll[Int](_ => intGen)(j =>
      Ch08.Phase3.forAll[List[Int]](intListGen)(loggedTest2(true)(i)(j)))), 2, 4))
  log("XXX = "
    + run(Ch08.Phase3.forAllAll[Int](_ => intGen)(i =>
    Ch08.Phase3.forAllAll[Int](_ => intGen)(j =>
      Ch08.Phase3.forAll[List[Int]](intListGen)(loggedTest2(false)(i)(j)))), 10, 100))

  println("* Every list of size 0 and 1 satisfyies predicate List.isSorted but not so lists of size 2 *")
  log("run(forAll[List[Int]](intListGen)(List.isSorted),0) = " + run(Ch08.Phase3.forAll[List[Int]](intListGen)(List.isSorted), 0))
  log("run(forAll[List[Int]](intListGen)(List.isSorted),1) = " + run(Ch08.Phase3.forAll[List[Int]](intListGen)(List.isSorted), 1))
  log("run(forAll[List[Int]](intListGen)(List.isSorted),2) = " + run(Ch08.Phase3.forAll[List[Int]](intListGen)(List.isSorted), 2))
  println("* Every list sorted using mergeSort or mergeSortPar satisfies predicate List.isSorted *")
  log("run(forAll[List[Int]](intListGen)(l=>List.isSorted(List.mergeSort(l))) = "
    + run(Ch08.Phase3.forAll[List[Int]](intListGen)(l => List.isSorted(List.mergeSort(l)))))
  log("run(forAll[List[Int]](intListGen)(l=>List.isSorted(Par.mergeSortPar(l)(esUnlimited).get))) = "
    + run(Ch08.Phase3.forAll[List[Int]](intListGen)(l => List.isSorted(Par.mergeSortPar(l)(esUnlimited).get))))

  println("*** Not finished yet ***")

}

