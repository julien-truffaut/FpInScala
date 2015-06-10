package fp


object Ch9 {

  trait Parsers[ParseError, Parser[+_]] { self =>

    def run[A](p: Parser[A])(input: String): Either[ParseError,A]

    def string(s: String): Parser[String]

    def char(c: Char): Parser[Char] =
      string(c.toString) map (_.charAt(0))

    def succeed[A](a: A): Parser[A] =
      string("") map (_ => a)

    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

    def many[A](p: Parser[A]): Parser[List[A]]

    def map[A,B](a: Parser[A])(f: A => B): Parser[B]

    def slice[A](p: Parser[A]): Parser[String]


    implicit def operators[A](p: Parser[A]): ParserOps[A] =
      ParserOps[A](p)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] =
      ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p,p2)
      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
      def many[B >: A]: Parser[List[B]] = self.many(p)
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
    }

    val numA: Parser[Int] = char('a').many.map(_.size)

    object Laws {
      import Ch8.Phase2._

      def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
        forAll(in)(s => run(p1)(s) == run(p2)(s))

      def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
        equal(p, p.map(a => a))(in)
    }
  }


}
