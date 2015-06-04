package fp




object Ch8 {

  // 8.1 To get used to thinking about testing in this way, come up with properties that specify the implementation
  // of a sum: List[Int] => Int function. You don’t have to write your properties down as executable ScalaCheck
  // code an informal description is fine.


  // 8.2 What properties specify a function that finds the maximum of a List[Int]?

  object Ch8Phase1 {

    // 8.3 Assuming the following representation of Prop, implement && as a method of Prop.

    trait Prop { def check: Boolean }

  }


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
      def unsized: SGen[A] = ???
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

    case class SGen[A](forSize: Int => Phase2.Gen[A])

    // 8.10 Implement helper functions for converting Gen to SGen. You can add this as a method on Gen.


    // 8.11 Not surprisingly, SGen at a minimum supports many of the same operations as Gen, and the
    // implementations are rather mechanical. Define some convenience functions on SGen that simply delegate
    // to the corresponding functions on Gen

    // 8.12 Implement a listOf combinator that doesn’t accept an explicit size. It should return an SGen
    // instead of a Gen. The implementation should generate lists of the requested size.
    def listOf[A](g: Gen[A]): SGen[List[A]] = ???

    // 8.13 Define listOf1 for generating nonempty lists, and then update your specification of max to use this generator.


    // 8.14 Write a property to verify the behavior of List.sorted (API docs link: http://mng.bz/ Pz86),
    // which you can use to sort (among other things) a List[Int].7 For instance, List(2,1,3).sorted is equal to List(1,2,3).


    // 8.15 Hard: A check property is easy to prove conclusively because the test just involves eval- uating the
    // Boolean argument. But some forAll properties can be proved as well. For instance, if the domain
    // of the property is Boolean, then there are really only two cases to test. If a property forAll(p)
    // passes for both p(true) and p(false), then it is proved. Some domains (like Boolean and Byte) are so small
    // that they can be exhaustively checked. And with sized generators, even infinite domains can be exhaustively
    // checked up to the maximum size. Automated testing is very useful, but it’s even better if we can automatically
    // prove our code correct. Modify our library to incorporate this kind of exhaustive checking of finite domains
    // and sized generators. This is less of an exercise and more of an extensive, open-ended design project.


    // 8.16 Hard: Write a richer generator for Par[Int], which builds more deeply nested parallel computations
    // than the simple ones we gave previously.

    // 8.17 Express the property about fork from chapter 7, that fork(x) == x.

    // 8.18 Come up with some other properties that takeWhile should satisfy. Can you think of a good property
    // expressing the relationship between takeWhile and dropWhile?


  }


}
