package gerard

object Chapter4Option {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = {
      flatMap(a => Some(f(a)))
    }

    // it's more natural to define map in terms of flatMap than vice-versa
    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(get) => f(get)
      case None      => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case Some(get) => get
      case None      => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = flatMap(_ => ob)

    def filter(f: A => Boolean): Option[A] = flatMap {
      a => if (f(a)) Some(a) else None
    }

    // note: anyone else who thinks that `fold` on Option
    // has a counter intuitive signature?
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  // 4.1 Implements Option functions, better use getOrElse and map


  // 4.2 Implement the variance function in terms of flatMap.
  // If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) for each element x in the sequence

  // implementation as the text says,
  // elegant but wrong, since we must divide by n-1, not n!!
  // see http://www.wikihow.com/Calculate-Variance or wolfram alpha...
  // also I don't like flatMap, a for-comprehension is much more readable
  def `variance wrong`(xs: Seq[Double]): Option[Double] = {
    for {
      m <- mean(xs)
      v <- mean(xs.map(x => math.pow(x - m, 2)))
    } yield {
      v
    }
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    for {
      m <- mean(xs)
      squared = xs.map(x => math.pow(x - m, 2))
    } yield {
      squared.sum / (xs.length - 1)
    }
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  // 4.3 Write a generic function map2 that combines two Option values using a binary function.
  // If either Option value is None, then the return value is too. Here is its signature:
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    //    a.flatMap {
    //      a1 => b.map {
    //        b1 => f(a1, b1)
    //      }
    //    }
    for {
      a1 <- a
      b1 <- b
    } yield {
      f(a1, b1)
    }
  }


  // 4.4 Write a function sequence that combines a list of Options into one Option containing
  // a list of all the Some values in the original list. If the original list contains None even
  // once, the result of the function should be None; otherwise the result should be Some with a
  // list of all the values. Here is its signature:
  def sequence[A](list: List[Option[A]]): Option[List[A]] = {
    list.foldRight[Option[List[A]]](Some(Nil)) {
      map2(_, _)(_ :: _)
    }
  }


  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(a map (i => Try(i.toInt)))


  // 4.5 Implement traverse. It’s straightforward to do using map and sequence, but try for a more efficient
  // implementation that only looks at the list once. In fact, implement sequence in terms of traverse.
  def traverse[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] = {
    list.foldRight[Option[List[B]]](Some(Nil)) {
      case (a, Some(l)) => f(a).map(_ :: l)
      case (_, None)    => None
    }
  }

}

object Chapter4Either {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = {
      flatMap(a => Right(f(a)))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(value)  => Left(value)
      case Right(value) => f(value)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
      flatMap(_ => b)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      for {
        a0 <- this
        b0 <- b
      } yield {
        f(a0, b0)
      }
    }
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
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(x => x)
  }

  // foldLeft since first error, not any error!
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldLeft[Either[E, List[B]]](Right(Nil)) {
      case (Right(acc), a)  =>
        f(a).map(_ :: acc)
      case (Left(value), _) =>
        Left(value)
    }.map(_.reverse)
  }


  case class Person(name: Name, age: Age)

  sealed class Name(val value: String)

  sealed class Age(val value: Int)

  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.") else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.") else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person)


  // 4.8 In this implementation, map2 is only able to report one error, even if both the name and the age are invalid.
  // What would you need to change in order to report both errors? Would you change map2 or the signature of mkPerson?
  // Or could you create a new data type that captures this requirement better than Either does, with some additional
  // structure? How would orElse, traverse, and sequence behave differently for that data type?

  sealed trait EitherValidation[+E, +A] {
    def map[B](f: A => B): EitherValidation[E, B] = {
      flatMap(a => OK(f(a)))
    }

    def flatMap[EE >: E, B](f: A => EitherValidation[EE, B]): EitherValidation[EE, B] = this match {
      case Errors(errors) => Errors(errors)
      case OK(value)      => f(value)
    }

    def orElse[EE >: E, B >: A](b: => EitherValidation[EE, B]): EitherValidation[EE, B] = {
      flatMap(_ => b)
    }

    def map2[EE >: E, B, C](that: EitherValidation[EE, B])(f: (A, B) => C): EitherValidation[EE, C] = {
      (this, that) match {
        case (Errors(e1), Errors(e2)) =>
          Errors(e1 ++ e2)
        case (OK(_), Errors(errors))  =>
          Errors(errors)
        case (Errors(errors), OK(_))  =>
          Errors(errors)
        case (OK(a), OK(b))           =>
          OK(f(a, b))
      }
    }
  }

  // traverse & sequence have no short-circuit path, it checks always all elements
  def traverse_[E, A, B](as: List[A])(f: A => EitherValidation[E, B]): EitherValidation[E, List[B]] = {
    as.foldRight[EitherValidation[E, List[B]]](OK(Nil)) {
      case (a, validation) =>
        f(a).map2(validation)(_ :: _)
    }
  }

  def sequence_[E, A](as: List[EitherValidation[E, A]]): EitherValidation[E, List[A]] = {
    traverse_(as)(x => x)
  }

  case class Errors[+E](errors: List[E]) extends EitherValidation[E, Nothing]

  case class OK[+A](value: A) extends EitherValidation[Nothing, A]

}
