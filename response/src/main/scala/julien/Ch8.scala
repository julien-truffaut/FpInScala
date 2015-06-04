package julien

import julien.Ch6.{RNG, SimpleRNG}
import julien.Ch8.Phase2

object Ch8 {

  // 8.1 To get used to thinking about testing in this way, come up with properties that specify the implementation
  // of a sum: List[Int] => Int function. You don’t have to write your properties down as executable ScalaCheck
  // code an informal description is fine.
  // xs: List[Int] => sum(xs) == sum(xs.reverse)
  // sum(Nil) == 0

  def sum(xs: List[Int]): Int = ???

  def splitProp(xs: List[Int], index: Int): Boolean = {
    val (left, right) = xs.splitAt(index)
    sum(left) + sum(right) == sum(xs)
  }


  // 8.2 What properties specify a function that finds the maximum of a List[Int]?

  def max(xs: List[Int]): Option[Int] = ???

  max(List(1,2,3)) == Some(3)

  def splitMaxProp(xs: List[Int], index: Int): Boolean = {
    val (left, right) = xs.splitAt(index)
    max(max(left).toList ++ max(right).toList) == max(xs)
  }

  // Option a = Some(a) | None

  def sortProp(xs: List[Int]): Boolean =
    xs.sorted.lastOption == max(xs)

  object Phase1 {

    // 8.3 Assuming the following representation of Prop, implement && as a method of Prop.

    trait Prop { self =>
      def check: Boolean

      def &&(other: Prop): Prop = new Prop {
        def check = self.check && other.check
      }
    }

    val prop1 : Prop = ???
    val prop2 : Prop = ???

    val prop3 = prop1 && prop2
  }


  object Phase2 {
    import Ch6.RNG
    import StateEx._
    type FailedCase = String
    type SuccessCount = Int

    // Either a b = Left a | Right b

    trait Prop {
      def check(minSuccessful: Int, rng: RNG): Either[(FailedCase, SuccessCount), SuccessCount]
    }

    case class Gen[A](sample: State[RNG,A]) {
      def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
      def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
      def listOfN(size: Int): Gen[List[A]] = Phase2.listOfN(size, this)
      def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(listOfN)
    }



    def forAll[A](a: Gen[A])(f: A => Boolean): Prop = new Prop {
      def check(minSuccessful: SuccessCount, rng: RNG): Either[(FailedCase, SuccessCount), SuccessCount] = {
        val as = listOfN(minSuccessful, a).sample.run(rng)._1
        as.dropWhile(f) match {
          case Nil    => Right(minSuccessful)
          case x :: xs => Left(x.toString -> (minSuccessful - xs.size - 1))
        }
      }
    }

    // 8.4 Implement Gen.choose using this representation of Gen. It should generate integers in the range
    // start to stopExclusive. Feel free to use functions you’ve already written.

    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      val sample: State[RNG, Int] = State[RNG, Int](rng => rng.nextInt).map(i => i % (stopExclusive - start) + start)
      Gen(sample)
    }

    // 8.5 Let’s see what else we can implement using this representation of Gen.
    // Try implementing unit, boolean, and listOfN.

    def unit[A](a: => A): Gen[A] = Gen(State[RNG, A](
      rng => (a, rng)
    ))


    def boolean: Gen[Boolean] = Gen(State[RNG, Boolean](
      rng => {
        val (i, rng2) = rng.nextInt
        (i % 2 == 0, rng2)
      }))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State[RNG, List[A]](
      rng => {
        if(n == 0) (Nil, rng)
        else {
          val (a, rng2) = g.sample.run(rng)
          val (as, rng3) = listOfN(n - 1, g).sample.run(rng2)
          (a :: as, rng3)
        }
      }))

    def listOfN2[A](n: Int, g: Gen[A]): Gen[List[A]] = {
      val states : List[State[RNG, A]] = List.fill(n)(g.sample)
      val s: State[RNG, List[A]] = State.sequence(states)
      Gen(s)
    }


    // 8.6 Implement flatMap, and then use it to implement this more dynamic version of listOfN.


    // 8.7 Implement union, for combining two generators of the same type into one,
    // by pulling values from each generator with equal likelihood.
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean.flatMap(if(_) g1 else g2)

    def int: Gen[Int] = Gen(State(_.nextInt))
    def nonNegativeInt: Gen[Int] =
      int.flatMap(i => if(i == Int.MinValue) nonNegativeInt else unit(i.abs))

    def double: Gen[Double] = nonNegativeInt.map(i => (i.toDouble / Int.MinValue).abs)

    // 8.8 Implement weighted, a version of union that accepts a weight for each Gen and generates values
    // from each Gen with probability proportional to its weight.
    def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] =
      double.flatMap { p =>
        val threshold = g1._2 / (g1._2 + g2._2)
        if (p > threshold) g2._1 else g1._1
      }

  }

  object Phase3 {
    import Ch6.RNG
    import StateEx._
    type FailedCase = String
    type SuccessCount = Int
    type TestCases = Int
    type MaxSize = Int


    sealed trait Result {
      def isFalsified: Boolean
    }

    case object Passed extends Result {
      override def isFalsified: Boolean = false
    }

    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
      def isFalsified = true
    }

    case class Prop(run: (MaxSize,TestCases,RNG) => Result){ self =>
      def &&(other: Prop): Prop = Prop(
        (max, n, rng) =>
          self.run(max,n,rng) match {
            case f: Falsified => f
            case Passed =>  other.run(max,n,rng)
        }
      )
    }

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
      forAll(g.forSize)(f)

    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (max, n, rng) =>
        val casesPerSize = (n + (max - 1)) / max
        val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
        val prop: Prop = props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
        prop.run(max, n, rng)
    }

    def forAll[A](a: Gen[A])(f: A => Boolean): Prop = Prop {
      (max, n, rng) =>
        val as = a.listOfN(n).sample.run(rng)._1
        as.dropWhile(f) match {
          case Nil     => Passed
          case x :: xs => Falsified(x.toString, n - xs.size - 1)
        }
      }

    case class Gen[A](sample: State[RNG,A]) {
      def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
      def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
      def listOfN(size: Int): Gen[List[A]] = Gen(State.sequence(List.fill(size)(sample)))
      def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(listOfN)
      def unsized: SGen[A] = SGen(_ => this)
    }

    object Gen {
      def choose(start: Int, stopExclusive: Int): Gen[Int] =
        Gen(State[RNG, Int](rng => rng.nextInt).map(i => i % (stopExclusive - start) + start))

      def unit[A](a: => A): Gen[A] = Gen(State[RNG, A](
        rng => (a, rng)
      ))

      def int: Gen[Int] = Gen(State(_.nextInt))
      def nonNegativeInt: Gen[Int] =
        int.flatMap(i => if(i == Int.MinValue) nonNegativeInt else unit(i.abs))
    }

    case class SGen[A](forSize: Int => Gen[A])

    object SGen {
      def listOf[A](g: Gen[A]): SGen[List[A]] =
        SGen(g.listOfN)

      def listOf1[A](g: Gen[A]): SGen[List[A]] =
        SGen(n => g.listOfN(n max 1))
    }

    // 8.12 Implement a listOf combinator that doesn’t accept an explicit size. It should return an SGen
    // instead of a Gen. The implementation should generate lists of the requested size.


    // 8.13 Define listOf1 for generating nonempty lists, and then update your specification of max to use this generator.


  }

}

object Ch8Phase2App extends App {
  import Ch8.Phase2._

  val rng = SimpleRNG(1000)

  val four: Gen[Int] = unit(4)
  val five: Gen[Int] = unit(5)

  print(Phase2.forAll(nonNegativeInt){i => println(i); i > 0}.check(100, rng))

}

object Ch8Phase3App extends App {
  import Ch8.Phase3._

  val smallInt = Gen.choose(-10,10)
  val maxProp = forAll(SGen.listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  println(maxProp.run(10, 5, SimpleRNG(1000)))

}
