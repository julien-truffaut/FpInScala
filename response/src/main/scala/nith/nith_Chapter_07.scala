package fp_nith

import util._
import java.util.Calendar
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger
import scala.Some

object Ch07 {

  object Phase1 {

    trait Par[A]

    def unit[A](a: => A): Par[A] = ???

    def get[A](a: Par[A]): A = ???

    // 7.1 Par.map2 is aPar new higher-order function for combining the result of two parallel computations.
    // What is its signature? Give the most general signature possible (don’t assume it works only for Int).
  }


  object Phase2 {

    trait Par[A]

    def unit[A](a: A): Par[A] = ???

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

    def fork[A](a: => Par[A]): Par[A] = ???

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](a: Par[A]): A = ???

    // 7.2 Before continuing, try to come up with representations for Par that make it possible
    // to implement the functions of our API.
  }

  object Phase3 {

    type Par[A] = ExecutorService => Future[A]

    object Par {
      def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

      private case class UnitFuture[A](get: A) extends Future[A] {
        def isDone = true

        def get(timeout: Long, units: TimeUnit) = get

        def isCancelled = false

        def cancel(evenIfRunning: Boolean): Boolean = false
      }

      def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

      def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
        (es: ExecutorService) => {
          val af = a(es)
          val bf = b(es)
          UnitFuture(f(af.get, bf.get))
        }

      def fork[A](aPar: => Par[A]): Par[A] =
        es => es.submit(new Callable[A] {
          def call = aPar(es).get
        })

      // 7.3 Hard: Fix the implementation of map2 so that it respects the contract of timeouts on Future.

      /**
       * This is the implementation from the official solution.
       * I did not understand the exercise, actually the whole template
       * It does not suffice to fix map2, the official solution introduces aPar new class and so on
       * Moreover some stuff from the standard Java library needs to be imported.
       * How shall one know this ? I thought the book does not require any prior experience with
       * Scala as it is written on page xviii but that seems to be utterly wrong.
       */
      case class Map2Future[A, B, C](a: Future[A], b: Future[B],
                                     f: (A, B) => C) extends Future[C] {
        @volatile var cache: Option[C] = None

        def isDone = cache.isDefined

        def isCancelled = a.isCancelled || b.isCancelled

        def cancel(evenIfRunning: Boolean) =
          a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

        def get = compute(Long.MaxValue)

        def get(timeout: Long, units: TimeUnit): C =
          compute(TimeUnit.MILLISECONDS.convert(timeout, units))

        private def compute(timeoutMs: Long): C = cache match {
          case Some(c) => c
          case None =>
            val start = System.currentTimeMillis
            val ar = a.get(timeoutMs, TimeUnit.MILLISECONDS)
            val stop = System.currentTimeMillis;
            val at = stop - start
            val br = b.get(timeoutMs - at, TimeUnit.MILLISECONDS)
            val ret = f(ar, br)
            cache = Some(ret)
            ret
        }
      }

      def map2Timeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
        (es: ExecutorService) => {
          val af: Future[A] = a(es)
          val bf: Future[B] = b(es)
          Map2Future(af, bf, f)
        }


      // 7.4 This API already enables aPar rich set of operations. Here’s aPar simple example: using lazyUnit,
      // write aPar function to convert any function A => B to one that evaluates its result asynchronously.

      def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

      def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

      // from the book
      def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

      // 7.5 Hard: Write this function, called sequence. No additional primitives are required. Do not call run.
      def nilPar[A]: Par[List[A]] = lazyUnit(List.Nil)

      def consPar[A](aPar: Par[A])(asPar: => Par[List[A]]): Par[List[A]] = map2(aPar, asPar)((h, t) => List.Cons(h, t))

      def appendPar[A](asPar1: => Par[List[A]])(asPar2: => Par[List[A]]): Par[List[A]] = map2(asPar1, asPar2)((as1, as2) => List.append(as1, as2))

      def sequence[A](ps: List[Par[A]]): Par[List[A]] = List.foldLeft[Par[A], Par[List[A]]](List.reverse(ps), nilPar)(consPar)

      def sequenceBal[A](aPars: List[Par[A]]): Par[List[A]] = fork {
        aPars match {
          case List.Nil => nilPar
          case List.Cons(aPar, List.Nil) => map2(aPar, nilPar)((h, t) => List.Cons(h, t))
          case _ => {
            log("...sequenceBal: aPars.length=" + List.length(aPars))
            val dimidia: (List[Par[A]], List[Par[A]]) = List.halve(aPars)
            appendPar(sequenceBal[A](dimidia._2))(sequenceBal[A](dimidia._1))
          }
        }
      }


      // book: Once we have sequence, we can complete our implementation of parMap:
      def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork {
        val fbs: List[Par[B]] = List.map[A,Par[B]](as)(asyncF(f))
        sequence(fbs)
      }

      def parMapBal[A, B](as: List[A])(f: A => B): Par[List[B]] = fork {
        val fbs: List[Par[B]] = List.map[A,Par[B]](as)(asyncF(f))
        sequenceBal(fbs)
      }

      // Two functions to filter aPar list with aPar little sleep
      def dormi(ms: Int): Unit = {
        log("...dormi: I am sleeping for " + ms + "ms !")
        Thread.sleep(ms)
        log("...dormi: I just woke up after " + ms + "ms .")
      }

      def filterSleep[A](as: List[A])(f: A => Boolean)(ms: Int): List[A] = {
        println("...filterSleep: as=" + List.myString(as) + " ms= " + ms)
        dormi(ms)
        List.reverse(List.foldLeft[A, List[A]](as, List.Nil)(a => l => if (f(a)) List.Cons(a, l) else l))
      }

      // 7.6 Implement parFilter, which filters elements of aPar list in parallel.
      def parFilterSequential[A](as: List[A])(f: A => Boolean): Par[List[A]] = unit(List.filter(as)(f))

      def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
        val dimidia: (List[A], List[A]) = List.halve(as)
        map2(lazyUnit(filterSleep(dimidia._2)(f)(3000)), lazyUnit(filterSleep(dimidia._1)(f)(3000)))(List.append)
      }


      // binary operator for lists
      def parBinOp[A](as: List[A])(z: A)(f: (A, A) => A): Par[A] = fork {
        as match {
          case List.Nil => lazyUnit(z)
          case List.Cons(a, List.Nil) => lazyUnit(f(a, z))
          case _ => {
            //          println("...parBinOp: z="+z+"   as="+List.myString(as))
            val dimidia: (List[A], List[A]) = List.halve(as)
            map2(parBinOp[A](dimidia._2)(z)(f), parBinOp[A](dimidia._1)(z)(f))(f)
          }
        }
      }


      final def sumIntList(ints: List[Int]): Par[Int] = parBinOp[Int](ints)(0)(_ + _)

      final def sumBigIntList(bigInts: List[BigInt]): Par[BigInt] = parBinOp[BigInt](bigInts)(0)(_ + _)

      final def prodIntList(ints: List[Int]): Par[Int] = parBinOp[Int](ints)(1)(_ * _)

      final def prodBigIntList(bigInts: List[BigInt]): Par[BigInt] = parBinOp[BigInt](bigInts)(1)(_ * _)

      final def maxIntList(ints: List[Int]): Par[Int] = parBinOp[Int](ints)(Int.MinValue)(_.max(_))

      // 7.11 Implement choiceN and then choice in terms of choiceN.
      def choiceN[A](n: Par[Int])(choices: List.Cons[Par[A]]): Par[A] = es => List.head(List.dropMod(choices)(n(es).get))(es)

      def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = choiceN(map[Boolean, Int](cond)(if (_) 1 else 0))(List.Cons(f, List.Cons(t, List.Nil)))

      def sumProdMaxIntList(ints: List[Int])(selector: Int): Par[Int] = choiceN(lazyUnit(selector))(List.Cons(sumIntList(ints), List(prodIntList(ints), maxIntList(ints))))

      def sumProdBigIntList(bigints: List[BigInt])(selector: Int): Par[BigInt] = choiceN(lazyUnit(selector))(List.Cons(sumBigIntList(bigints), List(prodBigIntList(bigints))))

      // 7.13 Implement this new primitive chooser, and then use it to implement choice and choiceN.
      def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => choices(pa(es).get)(es)

      // 7.14 Implement join. Can you see how to implement flatMap using join?
      // inner join
      def join[A](ppa: Par[Par[A]]): Par[A] = es => map[Par[A], A](ppa)(aPar => aPar(es).get)(es)

      def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = join[B](map[A, Par[B]](a)(f))

      // outer join
      def join2[A](ppa: Par[Par[A]]): Par[A] = es => ppa(es).get()(es)

      def flatMap2[A, B](a: Par[A])(f: A => Par[B]): Par[B] = join2[B](map[A, Par[B]](a)(f))

      // 7.14 And can you implement join using flatMap?
      def joinChooser[A](ppa: Par[Par[A]]): Par[A] = chooser[Par[A], A](ppa)(aPar => aPar)

      // additional stuff
      def mergeSortPar[A](as: List[A])(p: A => A => Boolean): Par[List[A]] = fork {
        as match {
          case List.Nil => lazyUnit(List.Nil)
          case List.Cons(a, List.Nil) => lazyUnit(List.Cons(a, List.Nil))
          case _ => {
            //          println("...parBinOp: z="+z+"   as="+List.myString(as))
            val dimidia: (List[A], List[A]) = List.halve(as)
            map2[List[A], List[A], List[A]](mergeSortPar[A](dimidia._1)(p), mergeSortPar[A](dimidia._2)(p))(List.merge[A](_)(_)(p))
          }
        }
      }

      def mergeSortPar(ints: List[Int]): Par[List[Int]] = mergeSortPar[Int](ints)(i => j => i < j)

      def equal[A](p: Par[A])(p2: Par[A]): Par[Boolean] = Par.map2(p, p2)(_ == _)

    }

  }

}

// Ch_07 {


object nith_Chapter_07 extends App {

  import util._
  import Ch07.Phase3.Par._

  lazy val intSign: (Boolean, Int) => Int = (p, n) => if (p) n else 0 - n

  // It would be nice if the red book did say how to create an ExecutorService.
  // I found this line in the comment of Executors.java but can speculate only
  // what it does.

  // Unser Exekutierer
  val numThreads: Int = 5
  val es1: ExecutorService = Executors.newFixedThreadPool(numThreads, new ThreadFactory {
    val counter = new AtomicInteger(0)

    def newThread(r: Runnable): Thread = {
      val t = new Thread(r, s"PAR-thread-${counter.getAndIncrement}")
      t.setDaemon(true)
      t
    }
  })
  val es2: ExecutorService = Executors.newFixedThreadPool(numThreads, new ThreadFactory {
    val counter = new AtomicInteger(0)

    def newThread(r: Runnable): Thread = {
      val t = new Thread(r, s"PAR-thread-${counter.getAndIncrement}")
      t.setDaemon(true)
      t
    }
  })

  // Lists
  val intList: Int => Int => List[Int] = start => card => if (card < 1) List.Nil else List.Cons(start, intList(start + 1)(card - 1))
  val intListList: Int => Int => List[List[Int]] = start => rowCard => if (rowCard < 1) List() else List.Cons(intList(start)(rowCard), intListList(start + rowCard)(rowCard - 1))
  val filterForEven: List[Int] => List[Int] = ints => filterSleep(ints)(_ % 2 == 0)(3000)

  // Streams
  lazy val oneStream: Ch05.Stream[Int] = Ch05.Stream.cons(1, oneStream)
  val isThereTwo: Ch05.Stream[Int] => Boolean = _.exists(_ == 2)
  lazy val nonTerminatingBool = isThereTwo(oneStream)
  lazy val nonTerminatingCall: Callable[Boolean] = new Callable[Boolean] {
    def call = nonTerminatingBool
  }
  // Pars
  lazy val twoPar: Ch07.Phase3.Par[Int] = unit(2)
  lazy val threePar: Ch07.Phase3.Par[Int] = unit(3)
  lazy val infinitePar: Ch07.Phase3.Par[Boolean] = execService => execService.submit[Boolean](nonTerminatingCall)

  println("****** Chapter_07 ******")
  println("Long.MaxValue     = %s".format(Long.MaxValue))
  println("intList(4)(8)     = " + List.myString(intList(4)(8)))
  println("intListList(4)(8) = " + List.myString(intListList(4)(8)))
  println("twoPar(es1).get    = " + twoPar(es1).get)
  println("threePar(es1).get  = " + threePar(es1).get)
  println("intSign: (Boolean, Int) => Int   =   (order, n) => if (order) n else 0 - n")


  println("\n** Exercises 7.3 and 7.4 with timeOut exceptions **")
  log("map2Timeout(twoPar,threePar)(_ * _)(es1).get = " + map2Timeout(twoPar, threePar)(_ * _)(es1).get)
  log("map2Timeout(twoPar,threePar)(_ * _)(es1).get(1,TimeUnit.SECONDS) = " + map2Timeout(twoPar, threePar)(_ * _)(es1).get(1, TimeUnit.SECONDS))
  try {
    log(map2Timeout(infinitePar, threePar)(intSign)(es1).get(2, TimeUnit.SECONDS))
  } catch {
    case e: Exception => logException(e)("map2Timeout(infinitePar,threePar)(intSign)(es1).get(2,TimeUnit.SECONDS)")
  }
  try {
    log(asyncF(isThereTwo)(oneStream)(es1).get(2, TimeUnit.SECONDS))
  } catch {
    case e: Exception => logException(e)("asyncF(isThereTwo)(oneStream)(es1).get(2,TimeUnit.SECONDS)")
  }


  println("\n** Exercise 7.5 **")
  log("sequence(List.Nil)(es2).get = " + sequence(List.Nil)(es2).get)
  log("sequence(List(twoPar,threePar))(es2).get = " + List.myString(sequence(List(twoPar, threePar))(es2).get) + "\n")
  log("* Let us fork 8 threads using parMap but our executor service has aPar pool of " + numThreads + " threads only *")
  log("Executor Service es2=" + es2 + "\n")
  log("parMap(intListList(4)(8))(filterForEven)(es2).get=" + List.myString(parMap[List[Int], List[Int]](intListList(4)(8))(filterForEven)(es2).get) + "\n")

  log("intListList(4)(8) = " + List.myString(intListList(4)(8)) + "\n")

  log("* Let us fork 8 threads using parMapBal and the unlimited executor Executors.newCachedThreadPool *")
  log("Executor Service esUnlimited=" + esUnlimited + "\n")
  log("parMapBal(intListList(4)(8))(filterForEven)(esUnlimited).get=" + List.myString(parMapBal[List[Int], List[Int]](intListList(4)(8))(filterForEven)(esUnlimited).get))
  println("\n** Exercise 7.6 **")
  log("parFilterSequential(List(0,1,2,3,4,5,6,7,8,9))(_%2==0)(es2).get) = " + List.myString(parFilterSequential(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))(_ % 2 == 0)(es2).get))
  log("parFilter(List(0,1,2,3,4,5,6,7,8,9))(n=>n%2==0)(es2).get         = " + List.myString(parFilter(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))(n => n % 2 == 0)(es2).get))
  log("sumIntList(List())(esUnlimited).get                              = " + sumIntList(List())(esUnlimited).get)
  log("sumIntList(List(42))(esUnlimited).get                            = " + sumIntList(List(42))(esUnlimited).get)
  log("sumIntList(List(1,2,3,4,5,6,7,8,9,10))(esUnlimited).get          = " + sumIntList(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(esUnlimited).get)
  log("prodIntList(List())(esUnlimited).get                             = " + prodIntList(List())(esUnlimited).get)
  log("prodIntList(List(42))(esUnlimited).get                           = " + prodIntList(List(42))(esUnlimited).get)
  log("prodIntList(List(1,2,3,4,5,6,7,8,9,10))(esUnlimited).get         = " + prodIntList(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(esUnlimited).get)
  println("* The following result is false:")
  logg("prodIntList(List(-2147483648,-150241704))(esUnlimited).get")(prodIntList(List(-2147483648, -150241704))(esUnlimited).get)
  logg("because of some overflow: (-2147483648)*(-150241704)")((-2147483648) * (-150241704))
  logg("but as BigInt: prodBigIntList(List(-2147483648,-150241704))(esUnlimited).get")(prodBigIntList(List(-2147483648, -150241704))(esUnlimited).get)
  log("maxIntList(List())(esUnlimited).get                              = " + maxIntList(List())(esUnlimited).get)
  log("maxIntList(List(42))(esUnlimited).get                            = " + maxIntList(List(42))(esUnlimited).get)
  log("maxIntList(List(1,2,3,4,5,6,7,8,9,10))(esUnlimited).get          = " + maxIntList(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(esUnlimited).get)
  Thread.sleep(100)


  println("\n** Exercise 7.11 **")
  log("sumProdMaxIntList(List(1,2,3,4,5,6,7,8,9,10))(0)(esUnlimited).get = " + sumProdMaxIntList(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(0)(esUnlimited).get)
  log("sumProdMaxIntList(List(1,2,3,4,5,6,7,8,9,10))(1)(esUnlimited).get = " + sumProdMaxIntList(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(1)(esUnlimited).get)
  log("sumProdMaxIntList(List(1,2,3,4,5,6,7,8,9,10))(2)(esUnlimited).get = " + sumProdMaxIntList(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(2)(esUnlimited).get)
  log("sumProdMaxIntList(List(1,2,3,4,5,6,7,8,9,10))(3)(esUnlimited).get = " + sumProdMaxIntList(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(3)(esUnlimited).get)

  log("choice(unit(false))(sumIntList(List(1,2,3,4,5,6,7,8,9,10)),prodIntList(List(1,2,3,4,5,6,7,8,9,10)))(esUnlimited).get = " + choice(unit(false))(sumIntList(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), prodIntList(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))(esUnlimited).get)
  log("choice(unit(true)) (sumIntList(List(1,2,3,4,5,6,7,8,9,10)),prodIntList(List(1,2,3,4,5,6,7,8,9,10)))(esUnlimited).get = " + choice(unit(true))(sumIntList(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)), prodIntList(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))(esUnlimited).get)

  println("\n** Exercise 7.13 **")
  log("chooser(unit(2))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + chooser(unit(2))(n => sumIntList(intList(n)(n)))(esUnlimited).get)
  log("chooser(unit(5))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + chooser(unit(5))(n => sumIntList(intList(n)(n)))(esUnlimited).get)
  log("chooser(unit(10))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + chooser(unit(10))(n => sumIntList(intList(n)(n)))(esUnlimited).get)

  println("\n** Exercise 7.14 **")
  log("flatMap users \"inner\" join, i.e. to convert ppa:Par[Par[A]] to pa:Par[A] it calculates the inner Par[A] to A")
  log("flatMap(unit(2))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + flatMap(unit(2))(n => sumIntList(intList(n)(n)))(esUnlimited).get)
  log("flatMap(unit(5))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + flatMap(unit(5))(n => sumIntList(intList(n)(n)))(esUnlimited).get)
  log("flatMap(unit(10))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + flatMap(unit(10))(n => sumIntList(intList(n)(n)))(esUnlimited).get)
  log("flatMap2 users \"outer\" join, i.e. to convert ppa:Par[Par[A]] to pa:Par[A] it calculates the outer Par[Par[A]] to Par[A]")
  log("flatMap2(unit(2))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + flatMap2(unit(2))(n => sumIntList(intList(n)(n)))(esUnlimited).get)
  log("flatMap2(unit(5))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + flatMap2(unit(5))(n => sumIntList(intList(n)(n)))(esUnlimited).get)
  log("flatMap2(unit(10))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + flatMap2(unit(10))(n => sumIntList(intList(n)(n)))(esUnlimited).get + "\n")

  println("\n** Additional Stuff **")
  log("mergeSortPar(List.integers(0)(99))(esUnlimited).get = " + List.myString(mergeSortPar(List.integers(0)(99))(esUnlimited).get))
  log("mergeSortPar(List.integers(99)(0))(esUnlimited).get = " + List.myString(mergeSortPar(List.integers(99)(0))(esUnlimited).get))
  logg("equal(unit(1 + 2))(unit(3))(esUnlimited).get")(equal(unit(1 + 2))(unit(3))(esUnlimited).get)
  println("\n****************************************************************************************************************************************\n")
  log("*** Shutting down the executor services es1, es2 and esunlimited ***\n")
  shutExecService(true)(es1)
  shutExecService(true)(es2)
  shutExecService(true)(esUnlimited)
}
