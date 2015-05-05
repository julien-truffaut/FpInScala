object Ch08 {

  // 8.1 To get used to thinking about testing in this way, come up with properties that specify the implementation
  // of a sum: List[Int] => Int function. You don’t have to write your properties down as executable ScalaCheck
  // code an informal description is fine.


  // 8.2 What properties specify a function that finds the maximum of a List[Int]?

  object Ch8Phase1 {

    // 8.3 Assuming the following representation of Prop, implement && as a method of Prop.

    trait Prop { def check: Boolean }

  }


  /*
  object Phase2 {
    import Ch6State._
    import Ch6.RNG
    type FailedCase = String
    type SuccessCount = Int

    trait Prop {
      def check: Either[(FailedCase, SuccessCount), SuccessCount]
    }

    case class Gen[A](sample: State[RNG,A]) {
      def flatMap[B](f: A => Gen[B]): Gen[B] = ???
      def listOfN(size: Gen[Int]): Gen[List[A]] = ???
    }

    def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

    // 8.4 Implement Gen.choose using this representation of Gen. It should generate integers in the range
    // start to stopExclusive. Feel free to use functions you’ve already written.

    def choose(start: Int, stopExclusive: Int): Gen[Int] = ???

    // 8.5 Let’s see what else we can implement using this representation of Gen.
    // Try implementing unit, boolean, and listOfN.

    def unit[A](a: => A): Gen[A] = ???

    def boolean: Gen[Boolean] = ???

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = ???

    // 8.6 Implement flatMap, and then use it to implement this more dynamic version of listOfN.


    // 8.7 Implement union, for combining two generators of the same type into one,
    // by pulling values from each generator with equal likelihood.
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = ???

    // 8.8 Implement weighted, a version of union that accepts a weight for each Gen and generates values
    // from each Gen with probability proportional to its weight.
    def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = ???

  }
*/

}


object nith_Chapter_08 extends App {

  println("****** Chapter_08 ******")
  println("\n* Some experssions using Int.MaxValue and Int.MinValue *")
  println("Int.MinValue = %s".format(Int.MinValue))
  println("Int.MaxValue =  %s".format(Int.MaxValue))
  println("Long.MinValue = %s".format(Long.MinValue))
  println("Long.MaxValue =  %s".format(Long.MaxValue))

  println("*** Not finished yet ***")

}

