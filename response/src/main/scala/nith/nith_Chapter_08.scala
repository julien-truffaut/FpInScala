import util.log
import Ch04_Option.{Option, Some}
import Ch05.{Stream, unfold, unfold2}
import Ch06.{double, doubleRandState, nonNegativeLessThanState, RandState, RNG, sequence, SimpleRNG, State, unitState}

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

    case class Gen[A](sample: State[RNG, A]) {
      // 8.6 Implement flatMap, and then use it to implement this more dynamic version of listOfN.
      def flatMap[B](f: A => Gen[B]): Gen[B] = new Gen[B](this.sample.flatMap[B](f(_).sample))

      def listOfN(size: Gen[Int]): Gen[List[A]] = {
        val runOfAs: RNG => (List[A], RNG) = rng => sequence[RNG, A](List.fill[RandState[A]](size.sample.run(rng)._1)(this.sample)).run(rng)
        val stateOfAs: State[RNG, List[A]] = new State[RNG, List[A]](runOfAs)
        new Gen[List[A]](stateOfAs)
      }
    }

    // 8.4 Implement Gen.choose using this representation of Gen. It should generate integers in the range
    // start to stopExclusive. Feel free to use functions you’ve already written.
    def choose(start: Int, stopExclusive: Int): Gen[Int] = new Gen[Int](nonNegativeLessThanState(stopExclusive - start).map[Int](_ + start))

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
//      final def myString:String = "Passed. Well Done !"
    }

    /*
    final def myString:String = this match {
      case Passed => "Passed. Well Done !"
      case Falsified(lFail,lSuc) => "Failure: successes="+lSuc+" failure="+lFail
    }
*/

    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
      final def isFalsified = true
//      final def myString:String = "Failure: successes="+successes+" failure="+failure
      }

    case class Prop(run: (TestCases, RNG) => Result) {
      // EXERCISE 8.9
      // Now that we have a representation of Prop, implement && and || for composing Prop values. Notice that in the
      // case of failure we don’t know which property was responsible, the left or the right. Can you devise a way of
      // handling this, perhaps by allowing Prop values to be assigned a tag or label which gets displayed in the event
      // of a failure?
      final def &&(p: Prop): Prop = {
        def newRunner (tc:TestCases, r: RNG) : Result = this.run(tc, r) match {
          case Passed => p.run(tc, r) match {
            case Falsified(rFail,rSuc) => new Falsified("LET prop passed but RIGHT Prop falsified: "+rFail, rSuc)
            case Passed => Passed
          }
          case Falsified(lFail,lSuc) => p.run(tc, r) match {
            case Passed => new Falsified("RIGHT Prop passed but LEFT Prop falsified: "+lFail, lSuc)
            case Falsified(rFail,rSuc) => new Falsified("BOTH Props falsified: (LEFT prop after "+lSuc+" test cases: "+lFail+", RIGHT prop after "+rSuc+" test cases: "+rFail+")", lSuc.min(rSuc))
          }
        }
            Prop(newRunner)
      }

      final def ||(p: Prop): Prop = {
        def newRunner (tc:TestCases, r: RNG) : Result = this.run(tc, r) match {
          case Passed => Passed
          case Falsified(lFail,lSuc) => p.run(tc, r) match {
            case Passed => Passed
            case Falsified(rFail,rSuc) => new Falsified("BOTH Props falsified: (LEFT prop after "+lSuc+" test cases: "+lFail+", RIGHT prop after "+rSuc+" test cases: "+rFail+")", lSuc.max(rSuc))
          }
        }
        Prop(newRunner)
      }
    }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
      (n, rng) => {
        val lazyIdentiy: (=> Int) => Int = n => n
        lazy val indexedAstream: Stream[(A, Int)] = randomStream[A](as)(rng).zip[Int](Stream(lazyIdentiy).take(n))
        lazy val mapli: (=> Tuple2[A, Int]) => Result = x => try {
          if (f(x._1)) Passed else Falsified(x._1.toString, x._2)
        } catch {
          case e: Exception => Falsified(buildMsg(x._1, e), x._2)
        }
        lazy val resultStream: Stream[Result] = indexedAstream.map[Result](mapli)
        resultStream.find(_.isFalsified).getOrElse(Passed)
      }
    }


  }

}

object nith_Chapter_08 extends App {

  println("****** Chapter_08 ******")
  val rng0: RNG = SimpleRNG(0)
  val rng1: RNG = SimpleRNG(0).nextInt._2
  val simpleRNGiterator: Int => Stream[(Int, RNG)] = count => unfold2[(Int, RNG), RNG](count)(rng0)(rng => {
    val (n, rng2) = rng.nextInt
    ((n, rng2), rng2)
  })


  log("rng0 = %s".format(rng0))
  log("rng1 = %s".format(rng1))
  log("simpleRNGiterator(9) = %s".format(simpleRNGiterator(9).myString))

  println("\n** Exercise 8.4 **")
  log("choose(0,42).sample.run(rng0)                                  = %s".format(Ch08.Phase2.choose(0, 42).sample.run(rng0)))
  log("choose(0,42).sample.run(choose(0,42).sample.run(rng0)._2))     = %s".format(Ch08.Phase2.choose(0, 42).sample.run(Ch08.Phase2.choose(0, 42).sample.run(rng0)._2)))
  log("choose(0.0,42.0).sample.run(rng0)                              = %s".format(Ch08.Phase2.choose(0.0, 42.0).sample.run(rng0)))
  log("choose(0.0,42.0).sample.run(choose(0,42).sample.run(rng0)._2)) = %s".format(Ch08.Phase2.choose(0.0, 42.0).sample.run(Ch08.Phase2.choose(0.0, 42.0).sample.run(rng0)._2)))

  println("\n** Exercise 8.5 **")
  log("unit(\"a\").sample.run(rng0)                          = %s".format(Ch08.Phase2.unit("a").sample.run(rng0)))
  log("unit(\"a\").sample.run(unit(\"a\").sample.run(rng0)._2) = %s".format(Ch08.Phase2.unit("a").sample.run(Ch08.Phase2.unit("a").sample.run(rng0)._2)))
  log("boolean.sample.run(rng0)                            = %s".format(Ch08.Phase2.boolean.sample.run(rng0)))
  log("boolean.sample.run(boolean.sample.run(rng0))        = %s".format(Ch08.Phase2.boolean.sample.run(Ch08.Phase2.boolean.sample.run(rng0)._2)))
  log("listOfN(10,unit(42)).sample.run(rng0)               = %s".format(Ch08.Phase2.listOfN(10, Ch08.Phase2.unit(42)).sample.run(rng0)))
  log("listOfN[Int](10,choose(0,42)).sample.run(rng0)      = %s".format(Ch08.Phase2.listOfN[Int](10, Ch08.Phase2.choose(0, 42)).sample.run(rng0)))

  println("\n** Exercise 8.6 **")
  log("unit(\"a\").listOfN(unit(10)).sample.run(rng0)        = %s".format(Ch08.Phase2.unit("a").listOfN(Ch08.Phase2.unit(10)).sample.run(rng0)))


  println("\n** Exercise 8.7 and 8.8**")
  log("union(choose(-42,-1),choose(1,42)).listOfN(unit(20)).sample.run(rng0))               = %s".format(Ch08.Phase2.union(Ch08.Phase2.choose(-42, -1), Ch08.Phase2.choose(1, 42)).listOfN(Ch08.Phase2.unit(20)).sample.run(rng0)))
  log("weighted((choose(-42,-1),0.0),(choose(1,42),1.0)).listOfN(unit(20)).sample.run(rng0) = %s".format(Ch08.Phase2.weighted((Ch08.Phase2.choose(-42, -1), 0.0), (Ch08.Phase2.choose(1, 42), 1.0)).listOfN(Ch08.Phase2.unit(20)).sample.run(rng0)))
  //  log("unfold(rng0)(rng => Some(double(rng))).take(20)                                      = %s".format(unfold[Double, RNG](rng0)(rng => Some(double(rng))).take(20).myString))
  log("weighted((choose(-42,-1),0.1),(choose(1,42),0.9)).listOfN(unit(20)).sample.run(rng0) = %s".format(Ch08.Phase2.weighted((Ch08.Phase2.choose(-42, -1), 0.1), (Ch08.Phase2.choose(1, 42), 0.9)).listOfN(Ch08.Phase2.unit(20)).sample.run(rng0)))
  log("weighted((choose(-42,-1),0.2),(choose(1,42),0.8)).listOfN(unit(20)).sample.run(rng0) = %s".format(Ch08.Phase2.weighted((Ch08.Phase2.choose(-42, -1), 0.2), (Ch08.Phase2.choose(1, 42), 0.8)).listOfN(Ch08.Phase2.unit(20)).sample.run(rng0)))
  log("weighted((choose(-42,-1),0.5),(choose(1,42),0.5)).listOfN(unit(20)).sample.run(rng0) = %s".format(Ch08.Phase2.weighted((Ch08.Phase2.choose(-42, -1), 0.5), (Ch08.Phase2.choose(1, 42), 0.5)).listOfN(Ch08.Phase2.unit(20)).sample.run(rng0)))

  println("\n** Exercise 8.9 **")
  log("choose(1,100).listOfN(unit(10)).sample.run(rng0)      = %s".format(Ch08.Phase2.choose(1,100).listOfN(Ch08.Phase2.unit(10)).sample.run(rng0)))
  log("choose(1,100))(n => n MOD 5 > 0).run(10,rng0)         = %s".format(Ch08.Phase2.forAll[Int](Ch08.Phase2.choose(1,100))(n => n%5>0).run(10,rng0)))
  log("choose(1,100))(n => n MOD 7 > 0).run(10,rng0)         = %s".format(Ch08.Phase2.forAll[Int](Ch08.Phase2.choose(1,100))(n => n%7>0).run(10,rng0)))
  log("conjunction &&                                        = %s".format(Ch08.Phase2.forAll[Int](Ch08.Phase2.choose(1,100))(n => n%5>0).&&(Ch08.Phase2.forAll[Int](Ch08.Phase2.choose(1,100))(n => n%7>0)).run(10,rng0)))
  log("disjunction ||                                        = %s".format(Ch08.Phase2.forAll[Int](Ch08.Phase2.choose(1,100))(n => n%5>0).||(Ch08.Phase2.forAll[Int](Ch08.Phase2.choose(1,100))(n => n%7>0)).run(10,rng0)))
  log("choose(1,100))(n => n MOD 5 > 0).run(10,rng0) && true = %s".format(Ch08.Phase2.forAll[Int](Ch08.Phase2.choose(1,100))(n => n%5>0).&&(Ch08.Phase2.forAll[Int](Ch08.Phase2.unit(0))(_ => true)).run(10,rng0)))
  log("choose(1,100))(n => n MOD 5 > 0).run(10,rng0) || true = %s".format(Ch08.Phase2.forAll[Int](Ch08.Phase2.choose(1,100))(n => n%5>0).||(Ch08.Phase2.forAll[Int](Ch08.Phase2.unit(0))(_ => true)).run(10,rng0)))

  println("*** Not finished yet ***")

}

