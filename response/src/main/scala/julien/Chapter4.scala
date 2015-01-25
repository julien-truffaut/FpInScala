package julien

object Chapter4Option {

  sealed trait Option[+A] {
    def cata[B](f: A => B, b: => B): B = this match {
      case None => b
      case Some(a) => f(a)
    }

    def map[B](f: A => B): Option[B] =
      cata(a => Some(f(a)), None)

    def flatMap[B](f: A => Option[B]): Option[B] =
      cata(f, None)

    def getOrElse[B >: A](default: => B): B =
      cata(identity, default)

    def orElse[B >: A](ob: => Option[B]): Option[B] =
      cata(_ => this, ob)

    def filter(f: A => Boolean): Option[A] =
      flatMap(a => if (f(a)) Some(a) else None)
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


  // 4.1 Implements Option functions, better use getOrElse and map


  // 4.2 Implement the variance function in terms of flatMap.
  // If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) for each element x in the sequence
  def variance(xs: Seq[Double]): Option[Double] = // TODO why flatMap ???
    mean(xs).map(m => xs.foldLeft(0: Double)((sum, x) => sum + math.pow(x - m, 2)))


  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  // 4.3 Write a generic function map2 that combines two Option values using a binary function.
  // If either Option value is None, then the return value is too. Here is its signature:
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(_a => b.map(f(_a, _)))


  // 4.4 Write a function sequence that combines a list of Options into one Option containing
  // a list of all the Some values in the original list. If the original list contains None even
  // once, the result of the function should be None; otherwise the result should be Some with a
  // list of all the values. Here is its signature:
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight(None: Option[List[A]])(map2(_, _)(_ :: _)) // underscore land


  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(a map (i => Try(i.toInt)))


  // 4.5 Implement traverse. It’s straightforward to do using map and sequence, but try for a more efficient
  // implementation that only looks at the list once. In fact, implement sequence in terms of traverse.
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight(None: Option[List[B]])((a, acc) => map2(f(a), acc)(_ :: _))

  def sequence2[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(identity)

}

object Chapter4Either{

  sealed trait Either[+E, +A] {
    def fold[B](f: E => B, g: A => B): B = this match {
      case Left(e)  => f(e)
      case Right(a) => g(a)
    }
    def map[B](f: A => B): Either[E, B] =
      fold(Left.apply, b => Right(f(b)))

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
      fold(Left.apply,f)

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] =
      fold(_ => b, _ => this)

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      flatMap(a => b.map(f(a, _)))
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = if (xs.isEmpty)
    Left("mean of empty list!")
  else
    Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }



  // 4.6 Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.


  // 4.7 Implement sequence and traverse for Either. These should return the first error that’s encountered, if there is one.
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight(Right(Nil): Either[E, List[A]])(_.map2(_)(_ :: _))

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight(Right(Nil): Either[E, List[B]])((a, acc) => f(a).map2(acc)(_ :: _))



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
