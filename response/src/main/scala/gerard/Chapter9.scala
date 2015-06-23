package gerard

import scala.annotation.tailrec
import scala.util.matching.Regex

object Chapter9 {

  object pre {

    trait Parsers[ParseError, Parser[+ _]] {
      self =>

      def run[A](p: Parser[A])(input: String): Either[ParseError, A]

      def char(c: Char): Parser[Char]

      def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

      implicit def string(s: String): Parser[String]

      implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

      implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
      ParserOps[String] = ParserOps(f(a))

      case class ParserOps[A](p: Parser[A]) {
        def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

        def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
      }

      // usage:
      run(or(string("abra"), string("cadabra")))("abra") == Right("abra")

      def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

      // A Parser[Int] that recognizes zero or more 'a' characters, and whose result
      // value is the number of 'a' characters it has seen. For instance, given "aa", the
      // parser results in 2; given "" or "b123" (a string not starting with 'a'), it results
      // in 0; and so on.
      def countOfAs: Parser[Int] = countOf(char('a'))

      // the idea is to concatenate parsers for increasing n in a lazy manner
      def countOf[A](p: Parser[A]): Parser[Int] = {
        def countNext(rep: Int = 0): Parser[Int] = {
          replace(listOfN[A](rep, p), rep) | countNext(rep + 1)
        }
        countNext()
      }

      // replaces the result of the parser, iff successful
      def replace[A, B](p: Parser[A], n: => B): Parser[B]

      // A Parser[Int] that recognizes one or more 'a' characters, and whose result
      // value is the number of 'a' characters it has seen. (Is this defined somehow in
      // terms of the same combinators as the parser for 'a' repeated zero or more
      // times?) The parser should fail when given a string without a starting 'a'. How
      // would you like to handle error reporting in this case? Could the API support giving
      // an explicit message like "Expected one or more 'a'" in the case of failure?
      def oneOrMore[A](p: Parser[A]): Parser[Int] = {
        error(andThen(p, countOf(p)) {
          case (value, i) => i + 1
        }, "Expected one or more 'a'")
      }

      // parses A, then parses B and combines the results with the function
      // (this is map2!)
      def andThen[A, B, C](s1: Parser[A], s2: Parser[B])(f: (A, B) => C): Parser[C]

      // fail a parser with a specific error message
      def error[A](p: Parser[A], e: => String): Parser[A]

      // A parser that recognizes zero or more 'a', followed by one or more 'b', and
      // which results in the pair of counts of characters seen. For instance, given "bbb",
      // we get (0,3), given "aaaab", we get (4,1), and so on.
      def asThenBs[A, B](p1: Parser[A], p2: Parser[B]): Parser[(Int, Int)] = {
        andThen(countOf(p1), countOf(p2)) {
          case (a, b) => a -> b
        }
      }
    }

  }

  trait Parsers[ParseError, Parser[+ _]] {
    self =>

    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

    implicit def string(s: String): Parser[String]

    implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

      def many: Parser[List[A]] = self.many(p)

      def map[B](f: A => B): Parser[B] = self.map(p)(f)

      // A followed by B
      def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

      def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

      // throw away result of left parser
      def **>[B](p2: Parser[B]): Parser[B] = self.product(p.slice, p2).map(_._2)

      // throw away result of right parser
      def <**[B](p2: Parser[B]): Parser[A] = self.product(p, p2.slice).map(_._1)

      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

      def slice: Parser[String] = self.slice(p)
    }

    def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

    def succeed[A](a: A): Parser[A]

    // Returns the portion of input inspected by p if successful
    def slice[A](p: Parser[A]): Parser[String]

    // 9.1 Using product, implement the now-familiar combinator map2 and then use this to
    // implement many1 in terms of many. Note that we could have chosen to make map2
    // primitive and defined product in terms of map2 as we’ve done in previous chapters.
    // The choice is up to you.
    def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = {
      p ** p2 map { case (a, b) => f(a, b) } // f.tupled !!!
    }

    def many1[A](p: Parser[A]): Parser[List[A]] = {
      map2(p, many(p))(_ :: _)
    }

    import gerard.Chapter8._
    import gerard.Chapter8.`8.4`._

    object Laws {
      def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
        forAll(in)(s => run(p1)(s) == run(p2)(s))

      def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
        equal(p, p.map(a => a))(in)

      // 9.2 Try coming up with laws to specify the behavior of product.
      def productLaw[A, B](p1: Parser[A], p2: Parser[B])(in: Gen[String]): Prop =
        equal(p1, p1 ** p1 map (_._1))(in) // same parser applied again should not parse anything anymore

      // associativity
      def productLaw2[A, B, C](a: Parser[A], b: Parser[B], c: Parser[C])(in: Gen[String]): Prop =
        equal(a ** (b ** c), (a ** b) ** c)(in)

    }

    // 9.3 Before continuing, see if you can define many in terms of or, map2, and succeed
    def many[A](p: Parser[A]): Parser[List[A]] = {
      val oneOrZero = p map (List(_)) or succeed(Nil)
      map2(oneOrZero, many(p))(_ ::: _)
      // however, if I look at this then we are calling many unconditionally thus resulting in a stack overflow...
      // guess we need sth that collapses the accumulated result as early as possible, like foldRight???
    }

    // 9.4 Using map2 and succeed, implement the listOfN combinator from earlier.
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
      if (n == 0) succeed(Nil) else map2(p, listOfN(n - 1, p))(_ :: _)
    }

    // 9.5 We could also deal with non-strictness with a separate combinator like we did in chapter
    // 7. Try this here and make the necessary changes to your existing combinators.
    // What do you think of that approach in this instance?
    // not sure how to implement this
    def lazyParser[A](p: => Parser[A]): Parser[A]

    def manyLazy[A](p: Parser[A]): Parser[List[A]] = {
      map2(p, lazyParser(manyLazy(p)))(_ :: _) or succeed(Nil)
    }

    // 9.6 Suppose we want to parse a single digit, like '4', followed by that many 'a' characters
    // (this sort of problem should feel familiar from previous chapters). Examples of
    // valid input are "0", "1a", "2aa", "4aaaa", and so on.
    def thatMany[A](p: Parser[A]): Parser[List[A]] = {
      val pDigit: Parser[String] = "[0-9]".r
      pDigit.flatMap {
        digit => listOfN(digit.toInt, p)
      }
    }

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    implicit def regex(r: Regex): Parser[String]

    // 9.7 Implement product and map2 in terms of flatMap
    def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)] = {
      for {
        a <- p
        b <- p2
      } yield a -> b
    }

    def map20[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = {
      for {
        a <- p
        b <- p2
      } yield f(a, b)
    }

    // 9.8 map is no longer primitive. Express it in terms of flatMap and/or other combinators
    def map[A, B](p: Parser[A])(f: A => B): Parser[B] = {
      p.flatMap(a => succeed(f(a)))
    }
  }

  trait JSON

  object JSON {

    case object JNull extends JSON

    case class JNumber(get: Double) extends JSON

    case class JString(get: String) extends JSON

    case class JBool(get: Boolean) extends JSON

    case class JArray(get: IndexedSeq[JSON]) extends JSON

    case class JObject(get: Map[String, JSON]) extends JSON

  }

  def jsonParser[Err, Parser[+ _]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._
    import JSON._

    val spaces: Parser[String] = char(' ').many.slice
    val newLines = char('\n').many.slice

    val skipWhiteSpace = spaces | newLines

    case class WhiteSpaceOps[A](p: Parser[A]) {
      // ignore any whitespace between two parsers
      def ***[B](p2: Parser[B]): Parser[(A, B)] = product(p, many(skipWhiteSpace) **> p2)
    }

    implicit def whiteSpaces[A](p: Parser[A]): WhiteSpaceOps[A] = WhiteSpaceOps[A](p)

    val sep: Parser[Char] = char(',')

    def many1Sep[A](p: Parser[A]): Parser[List[A]] = {
      map2(p, sep **> many(p))(_ :: _)
    }

    // this does not really do what I wanted... I'd like infix notation
    // the Scala combinators
    // just define another case class `~` for this but somehow I don't manage to get this working
    object ~> {
      def unapply[A, B](t: (A, B)): Option[B] = t match {
        case (a, b) => Some(b)
        case _      => None
      }
    }

    object <~ {
      def unapply[A, B](t: (A, B)): Option[A] = t match {
        case (a, b) => Some(a)
        case _      => None
      }
    }

    val number: Parser[JNumber] = """[0-9]*\.?[0-9]""".r.map(s => JNumber(s.toDouble))

    // that also looks shitty... string repetition, are you serious?
    // well I won't copy-paste the official solution here...
    val bool: Parser[JBool] = ("true" | "false").map {
      s => JBool(s.toLowerCase match {
        case "true" | "t"  => true
        case "false" | "f" => false
      })
    }

    val key: Parser[String] = """[a-zA-Z]+""".r

    val jnull: Parser[JSON] = string("null").map(_ => JNull)

    lazy val keyValue: Parser[(String, JSON)] = key *** (string(":") **> json)

    lazy val jobject: Parser[JObject] = (string("{") **> many1Sep(keyValue) <** "}")
      .map(kvp => JObject(kvp.toMap))

    lazy val jarray: Parser[JArray] = (string("[") **> many1Sep(json) <** "]")
      .map(values => JArray(values.toIndexedSeq))

    lazy val json: Parser[JSON] = number | bool | jarray | jobject

    json
  }

  trait `9.10`[ParseError, Parser[+ _]] extends Parsers[ParseError, Parser] {
    // too lazy to do that part... I guess something like
    def failWith[A](p: Parser[A])
                   (error: () => String): Parser[A] = ???

    // the operators like `**` could also return the last unsuccessful parser that was tried

    // Given a or b, if a fails on the input, do we always want to run b, or are there
    // cases where we might not want to?
    // => maybe if a and b start with the same non-terminal like an identifier and we just want to fail fast???

    // How do you want to handle reporting the location of errors?
    // => I'd prefer the clang style, to just mark the failing character:
    //               "abra cAdabra"
    // Expected "cadabra": ^

    // Given a or b, if a and b both fail on the input, might we want to support reporting
    // both errors?
    // => sometimes it might be handy to report what alternatives there are and just enumerate them...
    // (if there's some ambiguity)

    // And do we always want to report both errors, or do we want to
    // give the programmer a way to specify which of the two errors is reported?
    // => we could also pimp `map` and allow mapping of the errors
  }

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
    lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
      case -1        => offset + 1
      case lineStart => offset - lineStart
    }

    def next: String = input.drop(offset)

    def toError(msg: String): ParseError =
      ParseError(List((this, msg)))
  }

  case class ParseError(stack: List[(Location, String)])

  type Parser[+A] = Location => Result[A]

  trait Result[+A]

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]

  case class Failure(get: ParseError) extends Result[Nothing]

  // 9.13
  // Implement string, regex, succeed, and slice for this initial representation of
  // Parser. Note that slice is less efficient than it could be, since it must still construct a
  // value only to discard it.
  abstract class MyParser[+A]() {
    def run(input: String): Either[ParseError, A]
  }

  object MyParsers extends Parsers[ParseError, Parser] {
    // implementations of primitives go here
    def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
      p(Location(input)) match {
        case Success(get, charsConsumed) => Right(get)
        case Failure(error)              => Left(error)
      }

    def flatMap[A, B](p: Parser[A])(f: (A) => Parser[B]): Parser[B] = ???

    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A] = ???

    implicit def string(s: String): Parser[String] =
      (location: Location) =>
        if (location.next.startsWith(s))
          Success(s, s.length)
        else
          Failure(location.toError("Expected: " + s))

    def lazyParser[A](p: => Parser[A]): Parser[A] = ???

    implicit def regex(R: Regex): Parser[String] =
      (location: Location) =>
        location.next match {
          case R(s) => Success(s, s.length)
          case _    => Failure(location.toError("Expected: " + R.regex))
        }

    def succeed[A](a: A): Parser[A] = (location: Location) => Success(a, 0)

    def slice[A](p: Parser[A]): Parser[String] =
      (location: Location) => p(location) match {
        case Success(_, charsConsumed) =>
          Success(location.input.slice(location.offset, location.offset + charsConsumed), charsConsumed)
        case f: Failure                => f
      }
  }

}
