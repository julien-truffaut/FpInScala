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
    def op(fa: A => A, fb: A => A): A => A = {
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

    println(isOrdered(IndexedSeq(1, 3, 5, 7, 6)))
    println(isOrdered(IndexedSeq(1, 3, 5, 7, 16)))
  }

  // 10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    //    val bs = as.map(f)
    //    bs.fold(m.zero)(m.op)
    as.foldLeft(m.zero) {
      case (b, a) => m.op(b, f(a))
    }
  }

  // 10.6 foldLeft using foldMap
  def foldLeft[A, B](as: List[A], z: B)(op: (A, B) => B): B = {

    val m: Monoid[B => B] = endoMonoid[B]

    foldMap(as, m) {
      a: A => b: B => op(a, b)
    }(z)
  }

  // 10.7 Implement a foldMap for IndexedSeq.
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.size >= 2) {
      // split
      val (a, b) = v.splitAt(v.size / 2)
      val aM = foldMapV(a, m)(f)
      val bM = foldMapV(b, m)(f)
      m.op(aM, bM)
    } else {
      foldMap(v.toList, m)(f)
    }
  }

  // 10.8
  import `Chapter7 Rest`._
  import `Chapter7 Rest`.Par.Par

  def par[A](m: Monoid[A]): Monoid[Par[A]] = {
    new Monoid[Par[A]] {
      // op(op(x,y), z) == op(x, op(y,z))
      override def op(a: Par[A], b: Par[A]): Par[A] = {
        Par.map2(a, b)(m.op)
      }

      // op(x, zero) == x
      override def zero: Par[A] = Par.unit(m.zero)
    }
  }

  def parFoldMapFailed[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    if (v.isEmpty) {
      Par.unit(m.zero)
    } else if (v.size < 2) {
      par(m).op(Par.lazyUnit(f(v.head)), Par.unit(m.zero))
    } else {
      // split
      val (a, b) = v.splitAt(v.size / 2)
      val aM = parFoldMap(a, m)(f)
      val bM = parFoldMap(b, m)(f)
      par(m).op(aM, bM)
    }
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val mapped: Par[List[B]] = Par.parMap(v.toList)(f)
    Par.flatMap(mapped) {
      (a: List[B]) =>
        val b: Par[B] = foldMapV(a.toIndexedSeq, par(m))(b => Par.lazyUnit(b))
        b
    }
  }

  // 10.8 Hard: Use foldMap to detect whether a given IndexedSeq[Int] is ordered. You’ll need
  // to come up with a creative Monoid
  def isOrdered(s: IndexedSeq[Int]): Boolean = {

    // we store left / right intervals if ordered, None means not ordered
    val m = new Monoid[Option[(Int, Int)]] {

      // op(op(x,y), z) == op(x, op(y,z))
      override def op(a: Option[(Int, Int)],
                      b: Option[(Int, Int)]): Option[(Int, Int)] = {
        (a, b) match {
          case (Some(a0), None)                                 => Some(a0)
          case (None, Some(b0))                                 => Some(b0)
          case (None, None)                                     => None
          case (Some((left, leftEnd)), Some((right, rightEnd))) =>
            if (leftEnd <= right) {
              Some(left -> rightEnd)
            } else {
              None
            }
        }
      }

      // op(x, zero) == x
      override def zero: Option[(Int, Int)] = None
    }

    val intervalOpt = foldMap(s.toList, m)((v: Int) => Some(v -> v))
    intervalOpt.isDefined
  }


}
