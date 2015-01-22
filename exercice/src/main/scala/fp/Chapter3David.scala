import scala.annotation.tailrec

object Ch03Nel {

  sealed trait NeList[+A]

  case class Last[+A](elem: A) extends NeList[A]

  case class Cons[+A](head: A, tail: NeList[A]) extends NeList[A]

  object NeList {

    def apply[A](a: A, as: A*): NeList[A] = {
      if (as.isEmpty) Last(a)
      else Cons(a, apply(as(0), as.drop(1): _*))
    }

    // Exercise 3.2
    def tail[A](as: NeList[A]): Option[NeList[A]] = {
      as match {
        case Cons(_, xs) => Some(xs)
        case Last(_) => None
      }
    }

    // Exercise 3.3
    def setHead[A](as: NeList[A], newHead: A): Option[NeList[A]] = {
      as match {
        case Cons(_, xs) => Some(Cons(newHead, xs))
        case Last(_) => None
      }
    }

    // Exercise 3.4
    @tailrec
    def drop[A](as: NeList[A], n: Int): Option[NeList[A]] = {
      as match {
        case Cons(x, xs) if n != 0 => drop(xs, n - 1)
        case Last(x) if n != 0 => None
        case _ => Some(as)
      }
    }

    // Exercise 3.5
    def head[A](as: NeList[A]): A = {
      as match {
        case Cons(x, _) => x
        case Last(x) => x
      }
    }

    def dropWhile[A](as: NeList[A], pred: A => Boolean): Option[NeList[A]] = {
      as match {
        case Cons(x, xs) if pred(x) => dropWhile(xs, pred)
        case Last(x) if pred(x) => None
        case _ => Some(as)
      }
    }

    // Exercise 3.6
    def init[A](as: NeList[A]): Option[NeList[A]] = {
      as match {
        case Cons(x, xs) => {
          init(xs) match {
            case None => Some(Last(x))
            case Some(a) => Some(Cons(x, a))
          }
        }
        case Last(_) => None
      }
    }

    // Exercise 3.7
    def foldRight[A, B](as: NeList[A], z: B)(f: (A, B) => B): B =
      as match {
        case Last(x) => f(x, z)
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    // Exercise 3.9
    def size[A](as: NeList[A]): Int = {
      foldRight(as, 0)((_, b) => b + 1)
    }

    // Exercise 3.10
    @tailrec
    def foldLeft[A,B](as: NeList[A], z: B)(f: (B, A) => B): B = {
      as match {
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
        case Last(x) => f(z, x)
      }
    }

    def reduceLeft[A](as: NeList[A])(f: (A, A) => A): A = {
      as match {
        case Last(a) => a
        case Cons(x, xs) => foldLeft(xs, x)(f)
      }
    }

    // Exercise 3.11
    def sum[A: Numeric](as: NeList[A]): A = {
      val aNum = implicitly[Numeric[A]]
      foldLeft(as, aNum.zero)(aNum.plus)
    }

    def product[A: Numeric](as: NeList[A]): A = {
      val aNum = implicitly[Numeric[A]]
      foldLeft(as, aNum.one)(aNum.times)
    }

    def size2[A](as: NeList[A]): Int = {
      foldLeft(as, 0)((l, _) => l + 1)
    }

    // Exercise 3.12
    def reverse[A](as: NeList[A]): NeList[A] = {
      as match {
        case l@Last(a) => l
        case Cons(x, xs) => foldLeft(xs, Last(x): NeList[A])((t, h) => Cons(h, t))
      }
    }

    // Exercise 3.13
    def foldLeft2[A,B](as: NeList[A], z: B)(f: (B, A) => B): B = {
      ???
    }

    def foldRight2[A, B](as: NeList[A], z: B)(f: (A, B) => B): B = {
      foldLeft(reverse(as), z)((b, a) => f(a, b))
    }

    // Exercise 3.14
    def append[A](as1: NeList[A], as2: NeList[A]): NeList[A] = {
      foldRight(as1, as2)(Cons.apply)
    }

    // Exercise 3.15
    def reduceRight[A](as: NeList[A])(f: (A, A) => A): A = {
      as match {
        case Last(x) => x
        case Cons(x, xs) => f(x, reduceRight(xs)(f))
      }
    }
    def flatten[A](ass: NeList[NeList[A]]): NeList[A] = {
      reduceRight(ass)(append)
    }

    // Exercise 3.18
    def map[A, B](as: NeList[A])(f: A => B): NeList[B] = {
      as match {
        case Last(x) => Last(f(x))
        case Cons(x, xs) => Cons(f(x), map(xs)(f))
      }
    }

    // Exercise 3.19
    def filter[A](as: NeList[A])(f: A => Boolean): Option[NeList[A]] = {
      as match {
        case Last(x) if f(x) => Some(Last(x))
        case Last(x) if !f(x) => None
        case Cons(x, xs) => (f(x), filter(xs)(f)) match {
          case (true, Some(xxs)) => Some(Cons(x, xxs))
          case (true, None) => Some(Last(x))
          case (false, Some(xxs)) => Some(xxs)
          case (false, None) => None
        }
      }
    }

    // Exercise 3.20
    def flatMap[A,B](as: NeList[A])(f: A => NeList[B]): NeList[B] = {
      flatten(map(as)(f))
    }
  }

  // Exercise 3.21 not possible
}
