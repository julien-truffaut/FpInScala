package gerard

import gerard.Chapter6._

import scala.collection.immutable

object Chapter8 {

  object first {

    /*
    The library developed in this chapter goes through several iterations. This file is just the
    shell, which you can fill in and modify while working through the chapter.
    */
    trait Prop {
      outer =>

      def check: Boolean

      // 8.3
      def &&(p: Prop): Prop = new Prop {
        def check: Boolean = {
          outer.check && p.check
        }
      }
    }

  }

  // 8.1 write some properties for sum: List[Int] => Int
  // - if all elements have the same value => sum = length * value
  // => empty list => sum = 0
  // => sum(List(n)) == n
  // - filtering out elements with value 0 should result in the same sum

  // 8.2 maximum(List[Int])
  // - empty list => None or exception
  // - if all elements are the same, then maximum == any element
  // - if the list is sorted then maximum == List.head
  // - changing the order in the list should not change the outcome of maximum

  object `8.3` {

    trait Prop {
      outer =>

      def check: Boolean

      // 8.3
      def &&(p: Prop): Prop = new Prop {
        def check: Boolean = {
          outer.check && p.check
        }
      }
    }

  }

  object Gen {
    // 8.4
    def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen[Int] {
      State[RNG, Int](Chapter6.nonNegativeInt).map {
        i =>
          val scaled = start + (stopExclusive - start) * i.toLong / Int.MaxValue
          scaled.toInt
      }
    }

    // 8.4
    def choose(start: Double, stopExclusive: Double): Gen[Double] = Gen[Double] {
      State[RNG, Double](Chapter6.double).map {
        d =>
          val range = stopExclusive - start
          start + range * d
      }
    }

    // 8.5
    def unit[A](a: => A): Gen[A] = Gen(State.unit[RNG, A](a))

    def boolean: Gen[Boolean] = Gen[Boolean] {
      State[RNG, Int](_.nextInt).map(_ % 2 == 0)
    }

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State[RNG, List[A]] {
      rng =>
        List.fill(n)(g).foldLeft(List.empty[A] -> rng) {
          case ((acc, rng), gen) =>
            val (a, rng0) = gen.sample.run(rng)
            (a :: acc) -> rng0
        }
    })

    def sequence[A](gens: List[Gen[A]]): Gen[List[A]] = {
      gens.foldRight(unit[List[A]](List.empty[A])) {
        case (gen, acc) =>
          for {
            a <- gen
            as <- acc
          } yield {
            a :: as
          }
      }
    }

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
      boolean.flatMap {
        b => if (b) g1 else g2
      }
    }

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      choose(0, g1._2).flatMap {
        w1 =>
          choose(0, g2._2).flatMap {
            w2 => if (w1 > w2) g1._1 else g2._1
          }
      }
    }

  }

  // 8.11 Define some convenience functions on SGen that simply delegate to the corresponding functions on Gen.
  case class SGen[A](forSize: Int => Gen[A]) {
    def map[B](f: A => B): SGen[B] = SGen[B] {
      size: Int => forSize(size).map(f)
    }

    def flatMap[B](f: A => SGen[B]): SGen[B] = SGen[B] {
      size: Int =>
        forSize(size).flatMap(a => f(a).forSize(size))
    }
  }

  case class Gen[A](sample: State[RNG, A]) {
    // 8.6
    def flatMap0[B](f: A => Gen[B]): Gen[B] = Gen {
      State[RNG, B] {
        rng0 =>
          val (a, rng1) = sample.run(rng0)
          f(a).sample.run(rng1)
      }
    }

    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen {
      sample.flatMap(f(_).sample)
    }

    def map[B](f: A => B): Gen[B] = Gen {
      sample.map(f)
    }

    // 8.6
    def listOfN(size: Gen[Int], g: Gen[A]): Gen[List[A]] = {
      size.flatMap(n => Gen.sequence(List.fill(n)(g)))
    }

    // 8.10 Implement helper functions for converting Gen to SGen. You can add this as a method on Gen.
    def unsized: SGen[A] = SGen(_ => this)

  }

  object SGen {
    // 8.12
    // Implement a listOf combinator that doesn’t accept an explicit size. It should return an
    // SGen instead of a Gen. The implementation should generate lists of the requested size
    def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
      size: Int => Gen.listOfN(size, g)
    }

    // 8.13
    // Define listOf1 for generating nonempty lists, and then update your specification of
    // max to use this generator.
    def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen {
      size: Int => Gen.listOfN(size + 1, g)
    }
  }

  object `8.9` {
    type FailedCase = String
    type SuccessCount = Int
    type TestCases = Int

    sealed trait Result {
      def isFalsified: Boolean
    }

    case object Passed extends Result {
      def isFalsified = false
    }

    case class Falsified(failure: FailedCase,
                         successes: SuccessCount,
                         path: String) extends Result {
      def isFalsified = true
    }

    object Prop {
      def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
        (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
          case (a, i) => try {
            if (f(a)) Passed else Falsified(a.toString, i, "")
          } catch {
            case e: Exception => Falsified(buildMsg(a, e), i, "")
          }
        }.find(_.isFalsified).getOrElse(Passed)
      }

      import Stream._

      def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
        f(z) match {
          case Some((a, s)) => cons(a, unfold(s)(f))
          case None         => Empty
        }
      }

      def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
        unfold(rng)(rng => Some(g.sample.run(rng)))

      def buildMsg[A](s: A, e: Exception): String =
        s"test case: $s\n" +
          s"generated an exception: ${e.getMessage}\n" +
          s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
    }

    case class Prop(run: (TestCases, RNG) => Result,
                    label: String = "**") {
      def &&(p: Prop): Prop = Prop({
        case (testCases, rng) =>
          run(testCases, rng) match {
            case Passed                                =>
              p.run(testCases, rng) match {
                case Passed                  => Passed
                case f@Falsified(_, _, path) =>
                  f.copy(path = s"$label [<-passed] && ${p.label} [<-failed]")
              }
            case f@Falsified(failure, successes, path) =>
              f.copy(path = s"$label [<-failed] && ${p.label} [<-???]")
          }
      }, s"($label && ${p.label})")

      def ||(p: Prop): Prop = Prop({
        case (testCases, rng) =>
          run(testCases, rng) match {
            case Passed                              =>
              Passed
            case Falsified(failure, successes, path) =>
              p.run(testCases, rng) match {
                case Passed                  =>
                  Passed
                case f@Falsified(_, _, path) =>
                  f.copy(path = s"$path [<-failed] || ${p.label} [<-failed]")
              }
          }
      }, s"($label || ${p.label})")
    }

  }

  // Generating test cases up to a given maximum size
  object `8.4` {
    type MaxSize = Int
    type FailedCase = String
    type SuccessCount = Int
    type TestCases = Int

    sealed trait Result {
      def isFalsified: Boolean
    }

    case object Passed extends Result {
      def isFalsified = false
    }

    case class Falsified(failure: FailedCase,
                         successes: SuccessCount,
                         path: String) extends Result {
      def isFalsified = true
    }

    case class Prop(run: (MaxSize, TestCases, RNG) => Result,
                    label: String = "**") {
      def &&(p: Prop): Prop = Prop({
        case (maxSize, testCases, rng) =>
          run(maxSize, testCases, rng) match {
            case Passed                                =>
              p.run(maxSize, testCases, rng) match {
                case Passed                  => Passed
                case f@Falsified(_, _, path) =>
                  f.copy(path = s"$label [<-passed] && ${p.label} [<-failed]")
              }
            case f@Falsified(failure, successes, path) =>
              f.copy(path = s"$label [<-failed] && ${p.label} [<-???]")
          }
      }, s"($label && ${p.label})")

      def ||(p: Prop): Prop = Prop({
        case (maxSize, testCases, rng) =>
          run(maxSize, testCases, rng) match {
            case Passed                              =>
              Passed
            case Falsified(failure, successes, path) =>
              p.run(maxSize, testCases, rng) match {
                case Passed                  =>
                  Passed
                case f@Falsified(_, _, path) =>
                  f.copy(path = s"$path [<-failed] || ${p.label} [<-failed]")
              }
          }
      }, s"($label || ${p.label})")
    }

    import Stream._

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case Some((a, s)) => cons(a, unfold(s)(f))
        case None         => Empty
      }
    }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
      unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"


    def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
      forAll(g.forSize)(f)

    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (max, n, rng) =>
        val casesPerSize = (n + (max - 1)) / max
        val props: Stream[Prop] =
          Stream.from(0).take((n min max) + 1).map {
            i => forAll(g(i))(f)
          }
        val prop: Prop =
          props.map(p => Prop { (max, _, rng) =>
            p.run(max, casesPerSize, rng)
          }).toList.reduce(_ && _)
        prop.run(max, n, rng)
    }

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
      (n, casesPerSize, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i, "")
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i, "")
        }
      }.find(_.isFalsified).getOrElse(Passed)
    }

  }

  def main(args: Array[String]) {
    if (false) {
      import `8.9`._

      def report(result: Result) = result match {
        case Passed                              =>
          println("ok.")
        case Falsified(failure, successes, path) =>
          println(s"ko: found counter example <$failure> (after $successes successes): $path.")
      }

      report(Prop.forAll(Gen.choose(0, 10)) {
        b =>
          println(b)
          b < 8
      }.run(10, SimpleRNG(42)))

      println("*" * 40)

      report((Prop.forAll(Gen.choose(0, 10)) {
        b => b < 8
      } && Prop.forAll(Gen.choose(0, 10)) {
        b => b > 1
      }).run(10, SimpleRNG(42)))

      println("*" * 40)

      report((Prop.forAll(Gen.choose(0, 10)) {
        b => b < 11
      } && Prop.forAll(Gen.choose(0, 10)) {
        b => b > 1
      }).run(10, SimpleRNG(42)))
    }

    {
      import SGen._
      import `8.4`.{TestCases, forAll, Falsified, Passed, Prop}
      import Chapter6._

      def run(p: Prop,
              maxSize: Int = 100,
              testCases: Int = 100,
              rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
        p.run(maxSize, testCases, rng) match {
          case Falsified(msg, n, _) =>
            println(s"! Falsified after $n passed tests:\n $msg")
          case Passed               =>
            println(s"+ OK, passed $testCases tests.")
        }

      val smallInt = Gen.choose(-10, 10)
      val maxProp = forAll(listOf1(smallInt)) { ns =>
        val max = ns.max
        !ns.exists(_ > max)
      }
      run(maxProp)

      // 8.13
      // Write a property to verify the behavior of List.sorted
      val sortedProp = forAll(listOf(smallInt)) { ns =>
        val sorted: Array[Int] = ns.toArray
        java.util.Arrays.sort(sorted)
        ns.sorted == List(sorted: _*)
      }
      run(sortedProp)
    }
  }
}
