package fp_nith
import util._
import scala.annotation.tailrec

object Ch9 {

  trait Parsers[ParseError, Parser[+_]] { self =>

    def run[A](p: Parser[A])(input: String): Either[ParseError,A]

    def string(s: String): Parser[String]

    def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

    def succeed[A](a: A): Parser[A] = string("") map (_ => a)

    def or[A,B >: A](p1: Parser[A])(p2: Parser[B]): Parser[B]

    // 9.3 Hard: Before continuing, see if you can define many in terms of or, map2, and succeed.
/*
    def many[A](p: Parser[A]): Parser[List[A]] =  {
      val nilSuc:Parser[List[A]] = succeed[List[A]](List.Nil)
      val pSingleton: Parser[List[A]] = p.map2[List[A],List[A]](nilSuc)(a => as => List.Cons(a,as))
      def go(asp: Parser[List[A]]): Parser[List[A]] = pSingleton.or(go(p.map2[List[A],List[A]](asp)(a => as => List.Cons(a,as))))
      go(nilSuc)
    }
     */

    def many[A](p: Parser[A]): Parser[List[A]] =  {
      val nilSuc:Parser[List[A]] = succeed[List[A]](List.Nil)
      nilSuc.or(p.map2[List[A],List[A]](many[A](p))(a => as => List.Cons[A](a,as)))
    }

    //   def many[A](p: Parser[A]): Parser[List[A]] =  p.or(p.map2[List[A],List[A]](many[A](p))(a => as => List.Cons(a,as)))

    def map[A,B](a: Parser[A])(f: A => B): Parser[B]

    def slice[A](p: Parser[A]): Parser[String]

    def product[A,B](p1: Parser[A])(p2: Parser[B]): Parser[(A,B)]

    // 9.1 Using product, implement the now-familiar combinator map2 and then use this to implement many1 in terms of
    // many. Note that we could have chosen to make map2 primitive and defined product in terms of map2 as we’ve done
    // in previous chapters. The choice is up to you.
    def map2[A,B,C](p1: Parser[A])(p2: Parser[B])(f: A => B => C): Parser[C] =
      product[A,B](p1)(p2).map[C]( x => f(x._1)(x._2))

    def many1[A](p: Parser[A]): Parser[List[A]] = p.product[List[A]](many[A](p)).map[List[A]](_._2)


    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or[A,B](p)(p2)
      def |[B >: A](p2: Parser[B]): Parser[B] = or(p2)

      def many[B >: A]: Parser[List[B]] = self.many(p)
      def many1[B >: A]: Parser[List[B]] = self.many1(p)

      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def map2[B,C](p2: Parser[B])(f: A => B => C): Parser[C] = self.map2[A,B,C](p)(p2)(f)

      def product[B](p2: Parser[B]): Parser[(A,B)] = self.product[A,B](p)(p2)
      def **[B](p2: Parser[B]): Parser[(A,B)] = product[B](p2)
    }

    val numA: Parser[Int] = char('a').many.map(l => List.length(l))
    val numA1: Parser[Int] = char('a').many1.map(l => List.length(l))

    object Laws {
      import Ch08.Phase2.Gen
      import Ch08.Phase3.{forAll,forAllAll,Prop}

      final def equal[A](p1: Parser[A])(p2: Parser[A])(in: Int => Gen[String]): Prop =
        forAll(in)(s => run(p1)(s) == run(p2)(s))

      final def mapLaw[A](p: Parser[A])(in: Int => Gen[String]): Prop = equal(p)(p.map(a => a))(in)

      //9.2 Hard: Try coming up with laws to specify the behavior of product.
      final def prodAssociative[A](p1: Parser[A])(p2: Parser[A])(p3: Parser[A])(in: Int => Gen[String]): Prop =
        equal[(A,(A,A))](p1.**(p2.**(p3)))(p1.**(p2).**(p3).map[(A,(A,A))]( x => (x._1._1,(x._1._2,x._2))))(in)

      final def prodNeutralElement[A](p: Parser[A])(aGen: Int => Gen[A])(in: Int => Gen[String]): Prop =
        forAllAll[A](aGen)(a => equal[A](p)(p.**(succeed[A](a)).map[A](x => x._1))(in))

      final def prodCommutative[A](p1: Parser[A])(p2: Parser[A])(in: Int => Gen[String]): Prop =
        equal[(A,A)](p1.**(p2))(p1.**(p2))(in)

    }

  }


}


object nith_Chapter_09 extends App {

  println("****** Chapter_09 ******")

  println("\n** Exercise 9 XYZ **")
  logg("XXX")("Coming soon !")

  println()
  println("*** Not finished yet ***")
}

