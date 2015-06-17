package fp_nith

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ThreadFactory, ExecutorService, Executors}
import util._
import List._
import Ch04_Option.{Option, Some}
import Ch05.{Stream, unfold, unfold2}
import Ch06._
import Ch07.Phase3._

object Ch08 {

  // 8.1 To get used to thinking about testing in this way, come up with properties that specify the implementation
  // of aPar sum: List[Int] => Int function. You don’t have to write your properties down as executable ScalaCheck
  // code an informal description is fine.

  // 8.2 What properties specify aPar function that finds the maximum of aPar List[Int]?

  object Phase1 {

    // 8.3 Assuming the following representation of Prop, implement && as aPar method of Prop.
    trait Prop { self =>
      def check: Boolean

      def &&(p: Prop): Prop = new Prop {
        def check: Boolean = self.check && p.check
      }
    }

  }

  object Phase2 {
    type FailedCase = String
    type SuccessCount = Int

    case class Gen[+A](sample: State[RNG, A]) {
      // 8.6 Implement flatMap, and then use it to implement this more dynamic version of listOfN.
      final def flatMap[B](f: A => Gen[B]): Gen[B] = new Gen[B](this.sample.flatMap[B](f(_).sample))

      final def map[B](f: A => B): Gen[B] = new Gen[B](this.sample.map[B](f))

      final def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] = new Gen[C](this.sample.map2[B, C](g.sample)(f))

      def listOfN(size: Gen[Int]): Gen[List[A]] = {
        val runOfAs: RNG => (List[A], RNG) = rng => sequence[RNG, A](List.fill[RandState[A]](size.sample.run(rng)._1)(this.sample)).run(rng)
        val stateOfAs: State[RNG, List[A]] = new State[RNG, List[A]](runOfAs)
        new Gen[List[A]](stateOfAs)
      }

      // 8.10 Implement helper functions for converting Gen to SGen. You can add this as aPar method on Gen.
      def unsized: SGen[A] = new SGen[A](n => this)

      def **[B](g: Gen[B]): Gen[(A, B)] = this.map2[B, (A, B)](g)((a, b) => (a, b))

    }

    // 8.4 Implement Gen.choose using this representation of Gen. It should generate integers in the range
    // start to stopExclusive. Feel free to use functions you’ve already written.
    final def intGen: Gen[Int] = new Gen[Int](intRandState)

    final def choose(start: Int, stopExclusive: Int): Gen[Int] = new Gen[Int](nonNegativeLessThanState(stopExclusive - start).map[Int](_ + start))

    final def choose(start: Double, stopExclusive: Double): Gen[Double] = new Gen[Double](doubleRandState.map[Double](x => (stopExclusive - start) * x + start))

    // 8.5 Let’s see what else we can implement using this representation of Gen.
    // Try implementing unit, boolean, and listOfN.
    final def unit[A](a: => A): Gen[A] = new Gen[A](unitState[RNG, A](a))

    final def boolGen: Gen[Boolean] = new Gen[Boolean](new State[RNG, Boolean](rng => rng.nextInt match {
      case (n, r) => (n % 2 == 0, r)
    }))

    final def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      val stateOfAs: State[RNG, List[A]] = sequence[RNG, A](List.fill[RandState[A]](n)(g.sample))
      new Gen[List[A]](stateOfAs)
    }

    final def intListGen(n: Int): Gen[List[Int]] = listOfN(n, intGen)

    // 8.7 Implement union, for combining two generators of the same type into one,
    // by pulling values from each generator with equal likelihood.
    final def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolGen.flatMap[A](p => if (p) g1 else g2)

    final def unionLazy[A](g1: => Gen[A], g2: => Gen[A]): Gen[A] = boolGen.flatMap[A](p => if (p) g1 else g2)

    // 8.8 Implement weighted, aPar version of union that accepts aPar weight for each Gen and generates values
    // from each Gen with probability proportional to its weight.
    final def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = choose(0.0 - g1._2, g2._2).flatMap[A](x => if (x < 0) g1._1 else g2._1)

    type TestCases = Int

    sealed trait Result {
      def isFalsified: Boolean
    }

    case object Passed extends Result {
      final def isFalsified = false
    }

    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
      final def isFalsified = true
    }

    case class Prop(run: (TestCases, RNG) => Result) {
      // EXERCISE 8.9
      // Now that we have aPar representation of Prop, implement && and || for composing Prop values. Notice that in the
      // case of failure we don’t know which property was responsible, the left or the right. Can you devise aPar way of
      // handling this, perhaps by allowing Prop values to be assigned aPar tag or label which gets displayed in the event
      // of aPar failure?
      final def &&(p: Prop): Prop = {
        def newRunner(tc: TestCases, r: RNG): Result = this.run(tc, r) match {
          case Falsified(lFail, lSuc) => p.run(tc, r) match {
            case Falsified(rFail, rSuc) => new Falsified("Prop.&&: BOTH Props falsified: (LEFT FailedCase=" + lFail + " SuccessCount=" + lSuc + ", RIGHT FailedCase=" + rFail + " SuccessCount=" + rSuc + ")", lSuc.min(rSuc))
            case x => new Falsified("Prop.&&: RIGHT Prop " + x + " but LEFT Prop Falsified: FailedCase=" + lFail + " SuccessCount=" + lSuc, lSuc)
          }
          case x => p.run(tc, r) match {
            case Falsified(rFail, rSuc) => new Falsified("Prop.&&: LEFT prop " + x + "  but RIGHT Prop Falsified: FailedCase=" + rFail + " SuccessCount=" + rSuc, rSuc)
            case y => y
          }
        }
        Prop(newRunner)
      }

      final def ||(p: Prop): Prop = {
        def newRunner(tc: TestCases, r: RNG): Result = this.run(tc, r) match {
          case Falsified(lFail, lSuc) => p.run(tc, r) match {
            case Falsified(rFail, rSuc) => new Falsified("Prop.&&: BOTH Props Falsified: (LEFT FailedCase=" + lFail + " SuccessCount=" + lSuc + ", RIGHT FailedCase=" + rFail + " SuccessCount=" + rSuc + ")", lSuc.max(rSuc))
            case y => y
          }
          case x => x
        }
        Prop(newRunner)
      }
    }

    final def randomStream[A](length: Int)(g: Gen[A])(rng: RNG): Stream[A] = unfold2(length)(rng)(rng => g.sample.run(rng))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    def forAll[A](aGen: Gen[A])(P: A => Boolean): Prop = Prop {
      (n, rng) => {
        val lazyIdentity: (=> Int) => Int = n => n
        val indexedAstream: Stream[(A, Int)] = randomStream[A](n)(aGen)(rng).zip[Int](Stream(lazyIdentity))
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
      // 8.11 Not surprisingly, SGen at aPar minimum supports many of the same operations as Gen, and the implementations
      // are rather mechanical. Define some convenience functions on SGen that simply delegate to the corresponding
      // functions on Gen.
      def flatMap[B](f: A => SGen[B]): SGen[B] = new SGen[B](n => this.forSize(n).flatMap[B](a => f(a).forSize(n)))

      def listOfN(size: SGen[Int]): SGen[List[A]] = new SGen[List[A]](n => this.forSize(n).listOfN(size.forSize(n)))

    }

    // 8.12 Implement aPar listOf combinator that doesn’t accept an explicit size. It should return an SGen instead of aPar
    // Gen. The implementation should generate lists of the requested size.
    def listOf[A](g: Gen[A]): SGen[List[A]] = new SGen[List[A]](n => g.listOfN(unit(n)))

  }

  object Phase3 {

    import Ch07.Phase3.Par._
    import Phase2.{Gen, Passed, Falsified, Result, SGen, TestCases, boolGen, choose, intGen, intListGen, unit, weighted}

    case object Proved extends Result {
      final def isFalsified = false
    }

    // we define aPar strict order on Result as Falsified < Passed < Proved
    final def resultOrder(r1: Result)(r2: Result): Boolean = r1 match {
      case Falsified(_, _) => r2 match {
        case Falsified(_, _) => false
        case _ => true
      }
      case Passed => r2 match {
        case Proved => true
        case _ => false
      }
      case _ => false
    }

    final def min(r1: Result)(r2: Result): Result = if (resultOrder(r1)(r2)) r1 else r2

    final def max(r1: Result)(r2: Result): Result = min(r2)(r1)

    type MaxSize = Int

    case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
      // EXERCISE 8.9
      // Now that we have aPar representation of Prop, implement && and || for composing Prop values. Notice that in the
      // case of failure we don’t know which property was responsible, the left or the right. Can you devise aPar way of
      // handling this, perhaps by allowing Prop values to be assigned aPar tag or label which gets displayed in the event
      // of aPar failure?
      // COMMENT: It would be better to define && and || on type Result and let Prop inherit the functions
      // But for pedagogical reasons we follow the book.
      final def &&(p: Prop): Prop = {
        def newRunner(ms: MaxSize, tc: TestCases, r: RNG): Result = this.run(ms, tc, r) match {
          case Falsified(lFail, lSuc) => p.run(ms, tc, r) match {
            case Falsified(rFail, rSuc) => new Falsified("Prop.&&: BOTH Props Falsified: (LEFT FailedCase=[" + lFail + "] SuccessCount=" + lSuc + ", RIGHT FailedCase=[" + rFail + "] SuccessCount=" + rSuc + ")", lSuc.min(rSuc))
            case y => new Falsified("Prop.&&: RIGHT Prop " + y + " but LEFT Prop Falsified: FailedCase=[" + lFail + "] SuccessCount=" + lSuc, lSuc)
          }
          case x => p.run(ms, tc, r) match {
            case Falsified(rFail, rSuc) => new Falsified("Prop.&&: LEFT prop " + x + " but RIGHT Prop Falsified: FailedCase=[" + rFail + "] SuccessCount=" + rSuc, rSuc)
            case y => min(x)(y)
          }
        }
        Prop(newRunner)
      }

      final def ||(p: Prop): Prop = {
        def newRunner(ms: MaxSize, tc: TestCases, r: RNG): Result = this.run(ms, tc, r) match {
          case Falsified(lFail, lSuc) => p.run(ms, tc, r) match {
            case Falsified(rFail, rSuc) => new Falsified("Prop.&&: BOTH Props Falsified: (LEFT FailedCase=[" + lFail + "] SuccessCount=" + lSuc + ", RIGHT FailedCase=[" + rFail + "] SuccessCount=" + rSuc + ")", lSuc.max(rSuc))
            case y => y
          }
          case x => max(x)(p.run(ms, tc, r))
        }
        Prop(newRunner)
      }

      final def toBool(ms: MaxSize)(tc: TestCases)(r: RNG): Boolean = !this.run(ms, tc, r).isFalsified

    }

    // useful constants for the type Prop
    def alwaysFalsified: Prop = new Prop((_, _, _) => Falsified("always Falsified", 0))

    def alwaysPassed: Prop = new Prop((_, _, _) => Passed)

    def alwaysProved: Prop = new Prop((_, _, _) => Proved)


    def run(p: Prop,
            maxSize: Int = 128,
            testCases: Int = 3 * 128,
            rng: RNG = SimpleRNG(System.currentTimeMillis)
             ): String =
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) => "Falsified after " + n + " passed tests:\n" + msg
        case Passed => "Passed " + testCases + " tests."
        case Proved => "QED"
      }

    final def forAll[A](g: Int => Gen[A])(P: A => Boolean): Prop = Prop {
      (max, numTestCases, rng) =>
        val testCaseIndizes: List[Int] = List.integers(0)(numTestCases.min(max) - 1)
        val casesPerSize: Int = (numTestCases + max) / (max.abs + 1)
        val propSequence: Int => Phase2.Prop = i => Phase2.forAll(g(i))(P)
        val propPhase2List: List[Phase2.Prop] = List.map[Int, Phase2.Prop](testCaseIndizes)(propSequence)
        val propPhase3List: List[Prop] = List.map[Phase2.Prop, Phase3.Prop](propPhase2List)(p => Prop {
          (_, _, rng) => p.run(casesPerSize, rng)
        })
        val prop: Prop = List.foldLeft[Prop, Prop](propPhase3List, alwaysPassed)(prop1 => prop2 => prop2.&&(prop1))
        prop.run(max, numTestCases, rng)
    }

    final def forAllAll[A](g: Int => Gen[A])(P: A => Prop): Prop = Prop {
      (max, numTestCases, rng) => {
        val testCaseIndizes: List[Int] = List.integers(0)(numTestCases.min(max) - 1)
        val casesPerSize: Int = (numTestCases + max) / (max.abs + 1)
        val propSequence: Int => Phase2.Prop = i => Phase2.forAll(g(i))(a => P(a).toBool(max)(numTestCases)(rng))
        val propPhase2List: List[Phase2.Prop] = List.map[Int, Phase2.Prop](testCaseIndizes)(propSequence)
        val propPhase3List: List[Prop] = List.map[Phase2.Prop, Phase3.Prop](propPhase2List)(p => Prop {
          (_, _, rng) => p.run(casesPerSize, rng)
        })
        val prop: Prop = List.foldLeft[Prop, Prop](propPhase3List, alwaysPassed)(prop1 => prop2 => prop2.&&(prop1))
        //        logg("...Phase3.forAllAll: max=" + max + "\tnumTestCases=" + numTestCases + "\tcasesPerSize=" + casesPerSize
        //          + "\tpropPhase2List.size=" + List.length(propPhase2List) + "\tpropPhase3List.size")(List.length(propPhase3List))
        prop.run(max, numTestCases, rng)
      }
    }

    final def check(p: => Boolean): Prop = Prop {
      (_, _, _) => if (p) Proved else Falsified("()", 0)
    }

    final def genExecService(n: Int): Gen[ExecutorService] = {
      val weightForFixedPool: Double = 1 / (1 + n.abs)
      val threadPoolSize: Int = 4 + n.abs * n.abs
      //      logg("...genExecService: n="+n+"\tthreadPoolSize"+threadPoolSize+"\tweightForFixedPool")(weightForFixedPool)
      val genExService: Gen[ExecutorService] = weighted[ExecutorService]((unit(Executors.newFixedThreadPool(threadPoolSize)), weightForFixedPool), (unit(esUnlimited), 1 - weightForFixedPool))
      genExService
    }

    final def forAllPar[A](g: Int => Gen[A])(f: A => Par[Boolean]): Prop = {
      def exServiceA(n: Int): Gen[(ExecutorService, A)] = genExecService(n).**(g(n))
      forAll[(ExecutorService, A)](exServiceA) {
        case (exSe, a) => {
          val p: Boolean = f(a)(exSe).get
          shutExecService(false)(exSe)
          p
        }
      }
    }

    final def unitParGen(n: Int): Gen[Par[Int]] = choose(-n, n).map[Par[Int]](k => Par.unit(k))

    // 8.16 Hard: Write aPar richer generator for Par[Int], which builds more deeply nested parallel computations than the
    // simple ones we gave previously.

    final def parGen(rng: RNG)(n: Int): Gen[Par[Int]] = {
      val intList: List[Int] = intListGen(n).sample.run(rng)._1
      val sortIntListPar: Par[List[Int]] = mergeSortPar(intList)
      def chooseFun(k: Int): Par[Int] = {
        //        logg("...parGen.chooseFun: k"+k+"\tsumProdMaxIntList(intList)(k)=")(sumProdMaxIntList(intList)(k)(esUnlimited).get)
        //        logg("prodIntList")(prodIntList(intList)(esUnlimited).get)
        Par.flatMap[List[Int], Int](sortIntListPar)(l => sumProdMaxIntList(l)(k))
      }
      //      logg("...parGen: intList")(List.myString(intList))
      choose(-n, n).map[Par[Int]](chooseFun)
    }

    // 8.19 Hard: We want to generate a function that uses its argument in some way to select which Int to return. Can
    // you think of a good way of expressing this? This is a very openended and challenging design exercise. See what
    // you can discover about this problem and if there’s a nice general solution that you can incorporate into the
    // library we’ve developed so far.
    final def funGen[A, B](aGen: Gen[A])(bGen: Gen[B])(f: A => B)(n: Int): Gen[A => B] = {
      def funRun: RNG => ((A => B), RNG) = rng => {
        // Randomly generate a list of n pairs (a,b)
        val abGenList: Gen[List[(A, B)]] = Phase2.listOfN[(A, B)](n, aGen.**(bGen))
        val abListRng: (List[(A, B)], RNG) = abGenList.sample.run(rng)
        logg("...funGen: abList")(List.myString(abListRng._1))
        // Use the list to modify f
        val fun: A => B = modifyFun[A, B](f)(abListRng._1)
        (fun, abListRng._2)
      }
      def funSample: State[RNG, A => B] = new State[RNG, A => B](funRun)
      new Gen[A => B](funSample)
    }

    final def intFunGen: Int => Gen[Int => Int] = funGen[Int, Int](intGen)(intGen)(i => i)

    final def intPredEvenGen: Int => Gen[Int => Boolean] = funGen[Int, Boolean](intGen)(boolGen)(i => (i % 2 == 0))

  }

}

object nith_Chapter_08 extends App {

  import Ch07.Phase3.Par._
  import Ch08.Phase2.{boolGen, choose, forAll, Gen, intGen, intListGen, listOf, listOfN, Passed, Result, SGen, union, unit, weighted}
  import Ch08.Phase3.{check, forAllAll, forAllPar, intPredEvenGen, parGen, Prop, Proved, run, unitParGen}
  import java.util.concurrent._
  import java.util.concurrent.atomic.AtomicInteger

  println("****** Chapter_08 ******")
  val rng0: RNG = SimpleRNG(0)
  val rng1: RNG = SimpleRNG(0).nextInt._2
  val rngTime: RNG = SimpleRNG(System.nanoTime)
  val simpleRNGiterator: Int => Stream[(Int, RNG)] = count => unfold2[(Int, RNG), RNG](count)(rng0)(rng => {
    val (n, rng2) = rng.nextInt
    ((n, rng2), rng2)
  })

  logg("rng0")(rng0)
  logg("rng1")(rng1)
  logg("rngTime")(rngTime)
  logg("simpleRNGiterator(9)")(simpleRNGiterator(9).myString)

  println("\n** Exercise 8.4 **")
  logg("choose(0,42).sample.run(rng0)")(choose(0, 42).sample.run(rng0))
  logg("choose(0,42).sample.run(choose(0,42).sample.run(rng0)._2))")(choose(0, 42).sample.run(choose(0, 42).sample.run(rng0)._2))
  logg("choose(0.0,42.0).sample.run(rng0)")(choose(0.0, 42.0).sample.run(rng0))
  logg("choose(0.0,42.0).sample.run(choose(0,42).sample.run(rng0)._2))")(choose(0.0, 42.0).sample.run(choose(0.0, 42.0).sample.run(rng0)._2))

  println("\n** Exercise 8.5 **")
  logg("unit(\"aPar\").sample.run(rng0)")(unit("aPar").sample.run(rng0))
  logg("unit(\"aPar\").sample.run(unit(\"aPar\").sample.run(rng0)._2)")(unit("aPar").sample.run(unit("aPar").sample.run(rng0)._2))
  logg("boolean.sample.run(rng0)")(boolGen.sample.run(rng0))
  logg("boolean.sample.run(boolean.sample.run(rng0))")(boolGen.sample.run(boolGen.sample.run(rng0)._2))
  logg("listOfN(10,unit(42)).sample.run(rng0)")(listOfN(10, unit(42)).sample.run(rng0))
  logg("listOfN[Int](10,choose(0,42)).sample.run(rng0)")(listOfN[Int](10, choose(0, 42)).sample.run(rng0))

  println("\n** Exercise 8.6 **")
  logg("unit(\"aPar\").listOfN(unit(10)).sample.run(rng0)")(unit("aPar").listOfN(unit(10)).sample.run(rng0))
  logg("choose(0,42).listOfN(unit(10)).sample.run(rng0)")(choose(0, 42).listOfN(unit(10)).sample.run(rng0))
  logg("intGen.listOfN(unit(10)).sample.run(rng0)")(intGen.listOfN(unit(10)).sample.run(rng0))

  println("\n** Exercise 8.7 and 8.8**")
  logg("union(choose(-42,-1),choose(1,42)).listOfN(unit(20)).sample.run(rng0))")(union(choose(-42, -1), choose(1, 42)).listOfN(unit(20)).sample.run(rng0))
  logg("weighted((choose(-42,-1),0.0),(choose(1,42),1.0)).listOfN(unit(20)).sample.run(rng0)")(weighted((choose(-42, -1), 0.0), (choose(1, 42), 1.0)).listOfN(unit(20)).sample.run(rng0))
  logg("weighted((choose(-42,-1),0.1),(choose(1,42),0.9)).listOfN(unit(20)).sample.run(rng0)")(weighted((choose(-42, -1), 0.1), (choose(1, 42), 0.9)).listOfN(unit(20)).sample.run(rng0))
  logg("weighted((choose(-42,-1),0.2),(choose(1,42),0.8)).listOfN(unit(20)).sample.run(rng0)")(weighted((choose(-42, -1), 0.2), (choose(1, 42), 0.8)).listOfN(unit(20)).sample.run(rng0))
  logg("weighted((choose(-42,-1),0.5),(choose(1,42),0.5)).listOfN(unit(20)).sample.run(rng0)")(weighted((choose(-42, -1), 0.5), (choose(1, 42), 0.5)).listOfN(unit(20)).sample.run(rng0))

  println("\n** Exercise 8.9 **")
  logg("choose(1,100).listOfN(unit(10)).sample.run(rng0)")(choose(1, 100).listOfN(unit(10)).sample.run(rng0))
  logg("forAll[Int](choose(1,100))(n => n MOD 5 > 0).run(10,rng0)")(forAll[Int](choose(1, 100))(n => n % 5 > 0).run(10, rng0))
  logg("forAll[Int](choose(1,100))(n => n MOD 7 > 0).run(10,rng0)")(forAll[Int](choose(1, 100))(n => n % 7 > 0).run(10, rng0))
  logg("conjunction &&")(forAll[Int](choose(1, 100))(n => n % 5 > 0).&&(forAll[Int](choose(1, 100))(n => n % 7 > 0)).run(10, rng0))
  logg("disjunction ||")(forAll[Int](choose(1, 100))(n => n % 5 > 0).||(forAll[Int](choose(1, 100))(n => n % 7 > 0)).run(10, rng0))
  logg("forAll[Int](choose(1,100))(n => n MOD 5 > 0).run(10,rng0) && true")(forAll[Int](choose(1, 100))(n => n % 5 > 0).&&(forAll[Int](unit(0))(_ => true)).run(10, rng0))
  logg("forAll[Int](choose(1,100))(n => n MOD 5 > 0).run(10,rng0) || true")(forAll[Int](choose(1, 100))(n => n % 5 > 0).||(forAll[Int](unit(0))(_ => true)).run(10, rng0))
  logg("forAll[Int](choose(1,100))(n => n MOD 5 > 0).run(10,rng0) || true")(forAll[Int](choose(1, 100))(n => n % 5 > 0).||(forAll[Int](unit(0))(_ => true)).run(10, rng0))

  println("\n** Exercise 8.12 **")
  logg("listOf(unit(\"aPar\")).forSize(0).sample.run(rng0)")(listOf(unit("aPar")).forSize(0).sample.run(rng0))
  logg("listOf(choose(0,42)).forSize(0).sample.run(rng0)")(listOf(choose(0, 42)).forSize(0).sample.run(rng0))
  logg("listOf(unit(\"aPar\")).forSize(4).sample.run(rng0)")(listOf(unit("aPar")).forSize(4).sample.run(rng0))
  logg("listOf(choose(0,42)).forSize(4).sample.run(rng0)")(listOf(choose(0, 42)).forSize(4).sample.run(rng0))
  logg("listOf(unit(\"aPar\")).forSize(8).sample.run(rng0)")(listOf(unit("aPar")).forSize(8).sample.run(rng0))
  logg("listOf(choose(0,42)).forSize(8).sample.run(rng0)")(listOf(choose(0, 42)).forSize(8).sample.run(rng0))

  println("\n** Exercise 8.13 **")
  // aPar few more generators
  val maxIsTheBiggest: List[Int] => Boolean = (ns: List[Int]) => {
    val max: Int = List.max(ns)
    logg("...maxIsTheBiggest: testing !List.exists(" + List.myString(ns) + ")(_ > max)")(!List.exists(ns)(_ > max))
    !List.exists(ns)(_ > max)
  }
  val maxProp: Prop = Ch08.Phase3.forAll[List[Int]](intListGen)(maxIsTheBiggest)
  logg("maxIsTheBiggest(List.Nil)")(maxIsTheBiggest(List.Nil))
  logg("run(maxProp)")(run(maxProp, 5, 20))

  println("\n** Exercise 8.14 **")
  println("** Write aPar property to verify the behavior of List.sorted ...")
  println("* Testing predicate List.isSorted for Nil *")
  logg("List.isSorted(List.Nil)")(List.isSorted(List.Nil))
  println("\n* Testing predicate List.isSorted for the recursive built-up of lists using forAllAll *\n")
  println("* A few cases with logging *")
  val loggedTest1: Boolean => Int => List[Int] => Boolean = debug => n => l => {
    val result: Boolean = List.isSorted(Cons(n, l)) == (n <= List.min(l) && List.isSorted(l))
    if (debug) logg("... test function with l=" + List.myString(l) + "\tn=" + n + "\tList.min(l)=" + List.min(l)
      + "\tList.isSorted(l)=" + List.isSorted(l) + "\tn <= List.min(l)=" + (n <= List.min(l))
      + ":\tList.isSorted(" + List.myString(Cons(n, l)) + ")=" + List.isSorted(Cons(n, l)) + "\tresult")(result)
    result
  }
  logg("run(forAllAll[Int](_=>intGen)(n=> forAll[List[Int]](intListGen)(List.isSorted(Cons(n, l)) == (n <= List.min(l) && List.isSorted(l)))),2,4)")(run(forAllAll[Int](_ => intGen)(n => Ch08.Phase3.forAll[List[Int]](intListGen)(loggedTest1(true)(n))), 2, 6))

  println("* Many cases without logging *")
  logg("run(forAllAll[Int](_=>intGen)(n=> forAll[List[Int]](intListGen)(List.isSorted(Cons(n, l)) == (n <= List.min(l) && List.isSorted(l)))),2,4)")(run(forAllAll[Int](_ => intGen)(n => Ch08.Phase3.forAll[List[Int]](intListGen)(loggedTest1(false)(n))), 10, 100))

  println("* Different test for isSorted using 2 forAllAll *")
  val loggedTest2: Boolean => Int => Int => List[Int] => Boolean = debug => n => m => l => {
    val result: Boolean = List.isSorted(Cons(n, (Cons(m, l)))) == (n <= m && List.isSorted(Cons(m, l)))
    if (debug) logg("... test function with l=" + List.myString(l) + "\tn=" + n + "\tm=" + m
      + "\tList.isSorted(Cons(m, l))=" + List.isSorted(Cons(m, l)) + "\tn <= m=" + (n <= m)
      + ":\tList.isSorted(" + List.myString(Cons(n, (Cons(m, l)))) + ")=" + List.isSorted(Cons(n, (Cons(m, l)))) + "\tresult")(result)
    result
  }
  logg("run(forAllAll[Int](_=>intGen)(i=>forAllAll[Int](_=>intGen)(j=>forAll[List[Int]](intListGen)( List.isSorted(Cons(i,(Cons(j,l)))) == (i<=j && List.isSorted(Cons(j,l))))) ),2,4)")(run(forAllAll[Int](_ => intGen)(i =>
    forAllAll[Int](_ => intGen)(j =>
      Ch08.Phase3.forAll[List[Int]](intListGen)(loggedTest2(false)(i)(j)))), 2, 4))
  logg("run(forAllAll[Int](_=>intGen)(i=>forAllAll[Int](_=>intGen)(j=>forAll[List[Int]](intListGen)( List.isSorted(Cons(i,(Cons(j,l)))) == (i<=j && List.isSorted(Cons(j,l))))) ),16,512)")(run(forAllAll[Int](_ => intGen)(i =>
    forAllAll[Int](_ => intGen)(j =>
      Ch08.Phase3.forAll[List[Int]](intListGen)(loggedTest2(false)(i)(j)))), 10, 100))

  println("* Every list of size 0 and 1 satisfies predicate List.isSorted but not so lists of size 2 *")
  logg("run(forAll[List[Int]](intListGen)(List.isSorted),0)")(run(Ch08.Phase3.forAll[List[Int]](intListGen)(List.isSorted), 0))
  logg("run(forAll[List[Int]](intListGen)(List.isSorted),1)")(run(Ch08.Phase3.forAll[List[Int]](intListGen)(List.isSorted), 1))
  logg("run(forAll[List[Int]](intListGen)(List.isSorted),2)")(run(Ch08.Phase3.forAll[List[Int]](intListGen)(List.isSorted), 2))
  println("* Every list sorted using mergeSort or mergeSortPar satisfies predicate List.isSorted *")
  logg("run(forAll[List[Int]](intListGen)(l=>List.isSorted(List.mergeSort(l)))")(run(Ch08.Phase3.forAll[List[Int]](intListGen)(l => List.isSorted(List.mergeSort(l)))))
  logg("run(forAll[List[Int]](intListGen)(l=>List.isSorted(Par.mergeSortPar(l)(esUnlimited).get)))")(run(Ch08.Phase3.forAll[List[Int]](intListGen)(l => List.isSorted(Par.mergeSortPar(l)(esUnlimited).get))))

  println("\n* 8.4.2 Writing aPar test suite for parallel computations *")
  logg("run(check(List.isSorted(List.Nil)))")(run(check(List.isSorted(List.Nil))))
  logg("run(check(equal(Par.unit(1 + 2))(Par.unit(3))(esUnlimited).get)")(run(check(equal(Par.unit(1 + 2))(Par.unit(3))(esUnlimited).get)))
  logg("run(check(equal(Par.map(Par.unit(2))(_ + 1))(Par.unit(3))(esUnlimited).get))")(run(check(equal(Par.map(Par.unit(2))(_ + 1))(Par.unit(3))(esUnlimited).get)))

  logg("run(forAllPar(n => choose(-n,n))(n => equal(Par.map(Par.unit(n))(_ + 1))(Par.unit(n+1))))")(run(forAllPar[Int](n => choose(-n, n))(n => equal(Par.map(Par.unit(n))(_ + 1))(Par.unit(n + 1)))))
  logg("run(forAllAll[Int](k => choose(-k,k))(k => forAllPar[Int](n => choose(-n,n))(n => equal(Par.map(Par.unit(n))(_ + k))(Par.unit(n+k)))))")(run(forAllAll[Int](k => choose(-k, k))(k => forAllPar[Int](n => choose(-n, n))(n => equal(Par.map(Par.unit(n))(_ + k))(Par.unit(n + k))))))

  logg("run(forAllPar[Par[Int]](unitParGen)(nPar => equal[Int](Par.map(nPar)(y => y))(nPar)))")(run(forAllPar[Par[Int]](unitParGen)(nPar => equal[Int](Par.map(nPar)(y => y))(nPar))))

  println("\n** Exercise 8.16 **")
  println("* The following result is false:")
  logg("parGen(rngTime)(100).sample.run(rng0)._1(esUnlimited).get")(parGen(rngTime)(100).sample.run(rng0)._1(esUnlimited).get)
  logg("because of some overflow: prodIntList(List(-2147483648,-150241704))(esUnlimited).get")(prodIntList(List(-2147483648, -150241704))(esUnlimited).get)
  logg("because of some overflow: (-2147483648)*(-150241704)")((-2147483648) * (-150241704))
  logg("but as BigInt: prodBigIntList(List(-2147483648,-150241704))(esUnlimited).get")(prodBigIntList(List(-2147483648, -150241704))(esUnlimited).get)
  println()
  val testParMapIdentity: Boolean => Par[Int] => Par[Boolean] = (debug: Boolean) => (nPar: Par[Int]) => {
    if (debug) logg("...testParMapIdentity: nPar(esUnlimited).get")(nPar(esUnlimited).get)
    equal[Int](Par.map(nPar)(y => y))(nPar)
  }
  logg("run(forAllPar[Par[Int]](parGen)(nPar => equal(Par.map(nPar)(y => y))(nPar)),128,128)")(run(forAllPar[Par[Int]](parGen(rngTime))(testParMapIdentity(false)), 128, 128))


  println("\n** Exercise 8.17 **")
  val testParFork: Boolean => Par[Int] => Par[Boolean] = (debug: Boolean) => (nPar: Par[Int]) => {
    if (debug) logg("...testParFork: nPar(esUnlimited).get")(nPar(esUnlimited).get)
    equal[Int](Par.fork(nPar))(nPar)
  }
  logg("run(forAllPar[Par[Int]](parGen)(nPar => equal(Par.fork(nPar))(nPar)),128,128)")(run(forAllPar[Par[Int]](parGen(rngTime))(testParFork(false)), 128, 128))

  println("\n** Exercise 8.18 and 8.19 **")
  val testTakeOnly: Boolean => (Int => Boolean) => List[Int] => Boolean = debug => p => ints => {
    val result: Boolean = ints.takeOnly(p).forAll(p)
    if (debug) logg("... testTakeOnly: ints=" + List.myString(ints)
      + "\t ints.takeOnly(p1)=" + List.myString(ints.takeOnly(p))
      + "\tresult")(result)
    result
  }
  logg("run(forAllAll[Int=>Boolean](intPredGen)(p1 => forAll[List[Int]](intListGen)(testTakeOnly(true)(p1))),4,8)")(run(forAllAll[Int => Boolean](intPredEvenGen)(p => Ch08.Phase3.forAll[List[Int]](intListGen)(testTakeOnly(true)(p))), 4, 8))
  logg("run(forAllAll[Int=>Boolean](intPredGen)(p1 => forAll[List[Int]](intListGen)(testTakeOnly(false)(p1))),32,64)")(run(forAllAll[Int => Boolean](intPredEvenGen)(p => Ch08.Phase3.forAll[List[Int]](intListGen)(testTakeOnly(false)(p))), 32, 64))

  logg("run(forAllAll[Int => Boolean](intPredEvenGen)(p1 => forAll[List[Int]](intListGen)(ints => ints.dropWhile(p1).takeWhile(p1)==Nil)),32,64)")(run(forAllAll[Int => Boolean](intPredEvenGen)(p => Ch08.Phase3.forAll[List[Int]](intListGen)(ints => ints.dropWhile(p).takeWhile(p) == Nil)), 32, 64))
  logg("run(forAllAll[Int => Boolean](intPredEvenGen)(p1 => forAll[List[Int]](intListGen)(ints => ints.takeWhile(p1).dropWhile(p1)==Nil)),32,64)")(run(forAllAll[Int => Boolean](intPredEvenGen)(p => Ch08.Phase3.forAll[List[Int]](intListGen)(ints => ints.takeWhile(p).dropWhile(p) == Nil)), 32, 64))

  println()
  println("*** Not finished yet ***")

  log("*** Shutting down the executor services esunlimited ***\n")
  shutExecService(true)(esUnlimited)
}
