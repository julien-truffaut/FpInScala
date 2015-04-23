package gerard

import gerard.Chapter6._

object Chapter8 {

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

  object `8.4` {

    case class Gen[A](sample: State[RNG, A])

    object Gen {
      // 8.4
      def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen[Int] {
        State[RNG, Int] {
          rng =>
            val (i, rng0) = rng.nextInt
            val scaled = start + (stopExclusive - start) * i.toLong / Int.MaxValue
            scaled.toInt -> rng0
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
    }

  }

  object Prop {
    def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

  }

  object Gen {
    def unit[A](a: => A): Gen[A] = ???
  }

  trait Gen[A] {
    def map[A, B](f: A => B): Gen[B] = ???

    def flatMap[A, B](f: A => Gen[B]): Gen[B] = ???
  }

  trait SGen[+A] {

  }

}
