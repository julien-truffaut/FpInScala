package gerard

import gerard.Chapter6.SimpleRNG

object Chapter10 {

  trait Monoid[A] {
    // op(op(x,y), z) == op(x, op(y,z))
    def op(a: A, b: A): A

    // op(x, zero) == x
    // op(zero, x) == x
    def zero: A
  }

  // 10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {

    def op(a: Int, b: Int): Int = a + b

    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {

    def op(a: Int, b: Int): Int = a * b

    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean): Boolean = a || b

    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean): Boolean = a && b

    def zero: Boolean = true
  }

  // 10.2
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(a: Option[A], b: Option[A]): Option[A] = {
      a.orElse(b)
    }

    def zero: Option[A] = {
      None
    }
  }

  // 10.3
  def endoMonoid[A] = new Monoid[A => A] {
    // op(op(x,y), z) == op(x, op(y,z))
    def op(fa: (A) => A, fb: (A) => A): (A) => A = {
      a => fb(fa(a))
    }

    // op(x, zero) == x
    def zero: (A) => A = identity
  }

  // 10.4
  import Chapter8._
  import `8.9`.{Prop, Falsified, Passed, Result, TestCases}

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = Prop({
    case (testCases: TestCases, rng) =>

      val triple = for {
        x <- gen
        y <- gen
        z <- gen
      } yield {
        (x, y, z)
      }

      val leftZero = Prop.forAll(gen) {
        x =>
          import m._
          op(x, zero) == x
      }

      val rightZero = Prop.forAll(gen) {
        x =>
          import m._
          op(zero, x) == x
      }

      val associative: Prop = Prop.forAll(triple) {
        case (x, y, z) =>
          import m._
          op(op(x, y), z) == op(x, op(y, z))
      }

      val f = associative && leftZero && rightZero
      f.run(testCases, rng)
  })

  def main(args: Array[String]) {

    def report(result: Result) = result match {
      case Passed =>
        println("ok.")
      case Falsified(failure, successes, path) =>
        println(s"ko: found counter example <$failure> (after $successes successes): $path.")
    }

    report(monoidLaws(intMultiplication, Gen.choose(-20, 20)).run(100, SimpleRNG(42)))
  }
}
