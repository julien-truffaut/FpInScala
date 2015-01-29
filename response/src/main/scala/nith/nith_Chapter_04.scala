
object Chapter04_Option {

  sealed trait Option[+A] {
    // 4.1 Implements Option functions, better use getOrElse and map
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

    def orElse[B >: A](ob: => Option[B]): Option[B] = this.map[Option[A]](a => Some(a)).getOrElse(ob)

    def filter(f: A => Boolean): Option[A] = if (this.map(f).getOrElse(false)) this else None
  }

  // QUESTION: What does "get" mean here ?
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  // 4.2 Implement the variance function in terms of flatMap.
  // If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) for each element x in the sequence
  def variance(xs: Seq[Double]): Option[Double] = ???


  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  // 4.3 Write a generic function map2 that combines two Option values using a binary function.
  // If either Option value is None, then the return value is too. Here is its signature:
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???


  // 4.4 Write a function sequence that combines a list of Options into one Option containing
  // a list of all the Some values in the original list. If the original list contains None even
  // once, the result of the function should be None; otherwise the result should be Some with a
  // list of all the values. Here is its signature:
  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???


  /*
    def parseInts(a: List[String]): Option[List[Int]] =
      sequence(a map (i => Try(i.toInt)))
  */


  // 4.5 Implement traverse. It’s straightforward to do using map and sequence, but try for a more efficient
  // implementation that only looks at the list once. In fact, implement sequence in terms of traverse.
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???

}

object Chapter04_Either {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = ???

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = ???

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = ???

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = if (xs.isEmpty)
    Left("mean of empty list!")
  else
    Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }


  // 4.6 Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.


  // 4.7 Implement sequence and traverse for Either. These should return the first error that’s encountered, if there is one.
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = ???

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = ???


  case class Person(name: Name, age: Age)

  sealed class Name(val value: String)

  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.") else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.") else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))


  // 4.8 In this implementation, map2 is only able to report one error, even if both the name and the age are invalid.
  // What would you need to change in order to report both errors? Would you change map2 or the signature of mkPerson?
  // Or could you create a new data type that captures this requirement better than Either does, with some additional
  // structure? How would orElse, traverse, and sequence behave differently for that data type?


}

object nith_Chapter_04 extends App {

  val stringLength: String => Int = s => s.length
  val optionalStringLength: String => Option[Int] = s => Option(s.length)
  val evenStringLength: String => Boolean = s => s.length%2 == 0
  // QUESTION: I could also use Some as value. What is better ?
  // val optionalStringLength: String => Option[Int] = s => Some(s.length)

  println("************************")
  println("****** Chapter_04 ******")
  println("************************")

  print("** Exercise 4.1 **")
  // map
  println("None.map(stringLength) = " + None.map(stringLength))
  println("Option(\"\").map(stringLength) = " + Option("").map(stringLength))
  println("Option(\"abc\").map(stringLength) = " + Option("abc").map(stringLength))
  // getOrElse
  println("None.getOrElse(\"\") = " + None.getOrElse(""))
  println("None.getOrElse(None) = " + None.getOrElse(None))
  println("Option(\"\").getOrElse(\"\") = " + Option("").getOrElse(""))
  println("Option(\"abc\").getOrElse(\"\") = " + Option("abc").getOrElse(""))
  println("Option(Option(\"abc\")).getOrElse(\"\") = " + Option(Option("abc")).getOrElse(""))
  // map
  println("None.flatMap(optionalStringLength) = " + None.flatMap(optionalStringLength))
  println("Option(\"\").flatMap(optionalStringLength) = " + Option("").flatMap(optionalStringLength))
  println("Option(\"abc\").flatMap(optionalStringLength) = " + Option("abc").flatMap(optionalStringLength))
  // orElse
  println("None.orElse(None) = " + None.orElse(None))
  println("None.orElse(Option(1)) = " + None.orElse(Option(1)))
  println("None.orElse(Option(2.4)) = " + None.orElse(Option(2.4)))
  println("Option(42).orElse(Option(2.4)) = " + Option(42).orElse(Option(2.4)))
  println("Option(Option(42)).orElse(Option(2.4)) = " + Option(Option(42)).orElse(Option(2.4)))
  // filter
  println("None.filter(evenStringLength) = " + None.filter(evenStringLength))
  println("Option(\"\").filter(evenStringLength) = " + Option("").filter(evenStringLength))
  println("Option(\"abc\").filter(evenStringLength) = " + Option("abc").filter(evenStringLength))
  println("Option(\"abcd\").filter(evenStringLength) = " + Option("abcd").filter(evenStringLength))

}

