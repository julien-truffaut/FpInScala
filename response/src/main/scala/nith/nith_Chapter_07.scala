import java.util.Calendar
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicInteger

object Ch07 {

  object Phase1 {

    trait Par[A]

    def unit[A](a: => A): Par[A] = ???

    def get[A](a: Par[A]): A = ???

    // 7.1 Par.map2 is a new higher-order function for combining the result of two parallel computations.
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

      def fork[A](a: => Par[A]): Par[A] =
        es => es.submit(new Callable[A] {
          def call = a(es).get
        })

      // 7.3 Hard: Fix the implementation of map2 so that it respects the contract of timeouts on Future.

      /**
       * This is the implementation from the official solution.
       * I did not understand the exercise, actually the whole template
       * It does not suffice to fix map2, the official solution introduces a new class and so on
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


      // 7.4 This API already enables a rich set of operations. Here’s a simple example: using lazyUnit,
      // write a function to convert any function A => B to one that evaluates its result asynchronously.

      def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
      def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

      // from the book
      def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))
 
      // 7.5 Hard: Write this function, called sequence. No additional primitives are required. Do not call run.
      def nilPar[A]: Par[List[A]] = lazyUnit(List.Nil)
      def consPar[A](aPar:Par[A])(asPar: => Par[List[A]]):Par[List[A]] =  map2(aPar, asPar)((h,t) => List.Cons(h,t))
      def appendPar[A](asPar1: => Par[List[A]])(asPar2: => Par[List[A]]):Par[List[A]] =  map2(asPar1, asPar2)((as1,as2) => List.append(as1,as2))
      def sequence[A](ps: List[Par[A]]): Par[List[A]] = List.foldLeft[Par[A],Par[List[A]]](List.reverse(ps),nilPar)(consPar)
      def sequenceBal[A](aPars: List[Par[A]]): Par[List[A]] = fork { aPars match {
        case List.Nil => nilPar
        case List.Cons(aPar,List.Nil) => map2(aPar, nilPar)((h,t) => List.Cons(h,t))
        case _ => {
          println(Calendar.getInstance().getTime() + "...sequenceBal: aPars.length="+List.length(aPars))
          val dimidia:(List[Par[A]],List[Par[A]]) = List.halve(aPars)
          appendPar(sequenceBal[A](dimidia._2))(sequenceBal[A](dimidia._1))
        }
      }}


      // book: Once we have sequence, we can complete our implementation of parMap:
      def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] = fork {
        val fbs: List[Par[B]] = List.map(as)(asyncF(f))
        sequence(fbs)
      }

      def parMapBal[A,B](as: List[A])(f: A => B): Par[List[B]] = fork {
        val fbs: List[Par[B]] = List.map(as)(asyncF(f))
        sequenceBal(fbs)
      }

      // Two functions to filter a list with a little sleep
      def dormi(ms:Int):Unit = {
        println(Calendar.getInstance().getTime() + "...dormi: I am sleeping for "+ms+"ms !")
        Thread.sleep(ms)
        println(Calendar.getInstance().getTime() + "...dormi: I just woke up after "+ms+"ms .")
      }
      def filterSleep[A](as: List[A])(f: A => Boolean)(ms : Int): List[A] = {
        println("...filterSleep: as="+List.myString(as)+" ms= "+ms)
        dormi(ms)
        List.reverse(List.foldLeft[A, List[A]](as, List.Nil)(a => l => if (f(a)) List.Cons(a, l) else l))
      }

      // 7.6 Implement parFilter, which filters elements of a list in parallel.
      def parFilterSequential[A](as: List[A])(f: A => Boolean): Par[List[A]] = unit(List.filter(as)(f))
      def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
        val dimidia:(List[A],List[A]) = List.halve(as)
        map2(lazyUnit(filterSleep(dimidia._2)(f)(3000)),lazyUnit(filterSleep(dimidia._1)(f)(3000)))(List.append)
      }


      // general binary operator for lists
      def parBinOp[A](as: List[A])(z:A)(f: (A, A) => A): Par[A] = as match{
        case List.Nil => lazyUnit(z)
        case List.Cons(a,List.Nil) => lazyUnit(f(a,z))
        case _ => {
//          println("...parBinOp: z="+z+"   as="+List.myString(as))
          val dimidia:(List[A],List[A]) = List.halve(as)
          map2(parBinOp[A](dimidia._2)(z)(f),parBinOp[A](dimidia._1)(z)(f))(f)
        }
      }

      def sumIntList(ints: List[Int]) = parBinOp[Int](ints)(0)(_+_)
      def prodIntList(ints: List[Int]) = parBinOp[Int](ints)(1)(_*_)
      def maxIntList(ints: List[Int]) = parBinOp[Int](ints)(Int.MinValue)(_.max(_))

      // 7.11 Implement choiceN and then choice in terms of choiceN.
      def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => List.drop(choices, n(es).get-1) match {
        case List.Cons(h,t) => h(es)
      }
      def choice[A](cond: Par[Boolean])(t:Par[A] , f:Par[A]): Par[A] = choiceN(map[Boolean,Int](cond)(if (_) 2 else 1))(List(f,t))

      def sumProdMaxIntList(ints: List[Int])(selector:Int):Par[Int] = choiceN(lazyUnit(selector))(List(sumIntList(ints),prodIntList(ints),maxIntList(ints)))

      // 7.13 Implement this new primitive chooser, and then use it to implement choice and choiceN.
      def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => choices(pa(es).get)(es)

      // 7.14 Implement join. Can you see how to implement flatMap using join?
      // inner join
      def join[A](ppa: Par[Par[A]]): Par[A] = es => map[Par[A],A](ppa)(aPar => aPar(es).get)(es)
      def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = join[B](map[A,Par[B]](a)(f))
      // outer join
      def join2[A](ppa: Par[Par[A]]): Par[A] = es => ppa(es).get()(es)
      def flatMap2[A,B](a: Par[A])(f: A => Par[B]): Par[B] = join2[B](map[A,Par[B]](a)(f))
      // 7.14 And can you implement join using flatMap?
      def joinChooser[A](ppa: Par[Par[A]]): Par[A] = chooser[Par[A],A](ppa)(aPar => aPar)
    }
  }
}  // Ch_07 {


object nith_Chapter_07 extends App {

  val addTimeStamp: String => String = str => Calendar.getInstance().getTime() + "  " + str
  val log: Any => Unit = x => println(addTimeStamp(x.toString))
  val logException: Exception => ExecutorService => Any => Unit = except => execService => x => {
    System.err.println(addTimeStamp(x.toString + " = " + except))
  }
  lazy val intSign: (Boolean, Int) => Int = (p, n) => if (p) n else 0 - n

  // It would be nice if the red book did say how to create an ExecutorService.
  // I found this line in the comment of Executors.java but can speculate only
  // what it does.

  // Unser Exekutierer
  val numThreads:Int = 5
  val es1: ExecutorService = Executors.newFixedThreadPool(numThreads,new ThreadFactory {
    val counter = new AtomicInteger(0)
    def newThread(r: Runnable): Thread = {
      val t = new Thread(r, s"PAR-thread-${counter.getAndIncrement}")
      t.setDaemon(true)
      t
    }
  })
  val es2: ExecutorService = Executors.newFixedThreadPool(numThreads,new ThreadFactory {
    val counter = new AtomicInteger(0)
    def newThread(r: Runnable): Thread = {
      val t = new Thread(r, s"PAR-thread-${counter.getAndIncrement}")
      t.setDaemon(true)
      t
    }
  })
  val esUnlimited: ExecutorService = Executors.newCachedThreadPool(new ThreadFactory {
    val counter = new AtomicInteger(0)
    def newThread(r: Runnable): Thread = {
      val t = new Thread(r, s"PAR-thread-${counter.getAndIncrement}")
      t.setDaemon(true)
      t
    }
  })
  val shutExecService: ExecutorService => Unit = execService => {
    log("Shutting down now executor service\n"+execService.toString)
    log(execService.shutdown())
    log(execService.shutdownNow())
    if (!execService.awaitTermination(8, TimeUnit.SECONDS))
      System.err.println(addTimeStamp("Executor Service did not terminate \n"+execService.toString))
    else log("Executor terminated.\n"+execService.toString)
  }

  // Lists
  val intList: Int => Int => List[Int] = start => card => if (card<1) List.Nil else List.Cons(start,intList(start+1)(card-1))
  val intListList: Int => Int => List[List[Int]] = start => rowCard => if (rowCard<1) List() else List.Cons(intList(start)(rowCard),intListList(start+rowCard)(rowCard-1))
  val filterForEven: List[Int] => List[Int] = ints => Ch07.Phase3.Par.filterSleep(ints)(_%2==0)(3000)

  // Streams
  lazy val oneStream: Ch05.Stream[Int] = Ch05.Stream.cons(1, oneStream)
  val isThereTwo: Ch05.Stream[Int] => Boolean = _.exists(_ == 2)
  lazy val nonTerminatingBool = isThereTwo(oneStream)
  lazy val nonTerminatingCall: Callable[Boolean] = new Callable[Boolean] {def call = nonTerminatingBool}
  // Pars
  lazy val twoPar: Ch07.Phase3.Par[Int] = Ch07.Phase3.Par.unit(2)
  lazy val threePar: Ch07.Phase3.Par[Int] = Ch07.Phase3.Par.unit(3)
  lazy val infinitePar: Ch07.Phase3.Par[Boolean] = execService => execService.submit[Boolean](nonTerminatingCall)

  println("****** Chapter_07 ******")
  println("Long.MaxValue     = %s".format(Long.MaxValue))
  println("intList(4)(8)     = "+List.myString(intList(4)(8)))
  println("intListList(4)(8) = "+List.myString(intListList(4)(8)))
  println("twoPar(es1).get    = " + twoPar(es1).get)
  println("threePar(es1).get  = " + threePar(es1).get)
  println("intSign: (Boolean, Int) => Int   =   (p, n) => if (p) n else 0 - n")


  println("\n** Exercises 7.3 and 7.4 with timeOut exceptions **")
  log("map2Timeout(twoPar,threePar)(_ * _)(es1).get = " + Ch07.Phase3.Par.map2Timeout(twoPar,threePar)(_ * _)(es1).get)
  log("map2Timeout(twoPar,threePar)(_ * _)(es1).get(1,TimeUnit.SECONDS) = " + Ch07.Phase3.Par.map2Timeout(twoPar,threePar)(_ * _)(es1).get(1, TimeUnit.SECONDS))
  try {log(Ch07.Phase3.Par.map2Timeout(infinitePar,threePar)(intSign)(es1).get(2,TimeUnit.SECONDS))} catch {case e: Exception => logException(e)(es1)("map2Timeout(infinitePar,threePar)(intSign)(es1).get(2,TimeUnit.SECONDS)")}
  try {log(Ch07.Phase3.Par.asyncF(isThereTwo)(oneStream)(es1).get(2,TimeUnit.SECONDS))} catch {case e: Exception => logException(e)(es1)("asyncF(isThereTwo)(oneStream)(es1).get(2,TimeUnit.SECONDS)")}


  println("\n** Exercise 7.5 **")
  log("sequence(List.Nil)(es2).get = " + Ch07.Phase3.Par.sequence(List.Nil)(es2).get)
  log("sequence(List(twoPar,threePar))(es2).get = " + List.myString(Ch07.Phase3.Par.sequence(List(twoPar,threePar))(es2).get)+"\n")
  log("* Let us fork 8 threads using parMap but our executor service has a pool of "+numThreads+" threads only *")
  log("Executor Service es2="+es2+"\n")
  log("parMap(intListList(4)(8))(filterForEven)(es2).get="+List.myString(Ch07.Phase3.Par.parMap[List[Int],List[Int]](intListList(4)(8))(filterForEven)(es2).get)+"\n")

  log("intListList(4)(8) = "+List.myString(intListList(4)(8))+"\n")

  log("* Let us fork 8 threads using parMapBal and the unlimited executor Executors.newCachedThreadPool *")
  log("Executor Service esUnlimited="+esUnlimited+"\n")
  log("parMapBal(intListList(4)(8))(filterForEven)(esUnlimited).get="+List.myString(Ch07.Phase3.Par.parMapBal[List[Int],List[Int]](intListList(4)(8))(filterForEven)(esUnlimited).get))

  println("\n** Exercise 7.6 **")
  log("parFilterSequential(List(0,1,2,3,4,5,6,7,8,9))(_%2==0)(es2).get) = " + List.myString(Ch07.Phase3.Par.parFilterSequential(List(0,1,2,3,4,5,6,7,8,9))(_%2==0)(es2).get))
  log("parFilter(List(0,1,2,3,4,5,6,7,8,9))(n=>n%2==0)(es2).get         = " + List.myString(Ch07.Phase3.Par.parFilter(List(0,1,2,3,4,5,6,7,8,9))(n=>n%2==0)(es2).get))
  log("sumIntList(List())(esUnlimited).get                              = " + Ch07.Phase3.Par.sumIntList(List())(esUnlimited).get)
  log("sumIntList(List(42))(esUnlimited).get                            = " + Ch07.Phase3.Par.sumIntList(List(42))(esUnlimited).get)
  log("sumIntList(List(1,2,3,4,5,6,7,8,9,10))(esUnlimited).get          = " + Ch07.Phase3.Par.sumIntList(List(1,2,3,4,5,6,7,8,9,10))(esUnlimited).get)
  log("prodIntList(List())(esUnlimited).get                             = " + Ch07.Phase3.Par.prodIntList(List())(esUnlimited).get)
  log("prodIntList(List(42))(esUnlimited).get                           = " + Ch07.Phase3.Par.prodIntList(List(42))(esUnlimited).get)
  log("prodIntList(List(1,2,3,4,5,6,7,8,9,10))(esUnlimited).get         = " + Ch07.Phase3.Par.prodIntList(List(1,2,3,4,5,6,7,8,9,10))(esUnlimited).get)
  log("maxIntList(List())(esUnlimited).get                              = " + Ch07.Phase3.Par.maxIntList(List())(esUnlimited).get)
  log("maxIntList(List(42))(esUnlimited).get                            = " + Ch07.Phase3.Par.maxIntList(List(42))(esUnlimited).get)
  log("maxIntList(List(1,2,3,4,5,6,7,8,9,10))(esUnlimited).get          = " + Ch07.Phase3.Par.maxIntList(List(1,2,3,4,5,6,7,8,9,10))(esUnlimited).get)
  Thread.sleep(100)
  println("\n** Exercise 7.11 **")
  log("sumProdMaxIntList(List(1,2,3,4,5,6,7,8,9,10))(1)(esUnlimited).get = " + Ch07.Phase3.Par.sumProdMaxIntList(List(1,2,3,4,5,6,7,8,9,10))(1)(esUnlimited).get)
  log("sumProdMaxIntList(List(1,2,3,4,5,6,7,8,9,10))(2)(esUnlimited).get = " + Ch07.Phase3.Par.sumProdMaxIntList(List(1,2,3,4,5,6,7,8,9,10))(2)(esUnlimited).get)
  log("sumProdMaxIntList(List(1,2,3,4,5,6,7,8,9,10))(3)(esUnlimited).get = " + Ch07.Phase3.Par.sumProdMaxIntList(List(1,2,3,4,5,6,7,8,9,10))(3)(esUnlimited).get)
  try {log(Ch07.Phase3.Par.sumProdMaxIntList(List(1,2,3,4,5,6,7,8,9,10))(4)(es1).get)} catch {case e: Exception => logException(e)(es1)("sumProdMaxIntList(List(1,2,3,4,5,6,7,8,9,10))(4)(es1).get")}
  log("choice(unit(false))(sumIntList(List(1,2,3,4,5,6,7,8,9,10)),prodIntList(List(1,2,3,4,5,6,7,8,9,10)))(esUnlimited).get = " + Ch07.Phase3.Par.choice(Ch07.Phase3.Par.unit(false))(Ch07.Phase3.Par.sumIntList(List(1,2,3,4,5,6,7,8,9,10)), Ch07.Phase3.Par.prodIntList(List(1,2,3,4,5,6,7,8,9,10)))(esUnlimited).get)
  log("choice(unit(true)) (sumIntList(List(1,2,3,4,5,6,7,8,9,10)),prodIntList(List(1,2,3,4,5,6,7,8,9,10)))(esUnlimited).get = " + Ch07.Phase3.Par.choice(Ch07.Phase3.Par.unit(true))(Ch07.Phase3.Par.sumIntList(List(1,2,3,4,5,6,7,8,9,10)), Ch07.Phase3.Par.prodIntList(List(1,2,3,4,5,6,7,8,9,10)))(esUnlimited).get)

  println("\n** Exercise 7.13 **")
  log("chooser(unit(2))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + Ch07.Phase3.Par.chooser(Ch07.Phase3.Par.unit(2))(n => Ch07.Phase3.Par.sumIntList(intList(n)(n)))(esUnlimited).get)
  log("chooser(unit(5))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + Ch07.Phase3.Par.chooser(Ch07.Phase3.Par.unit(5))(n => Ch07.Phase3.Par.sumIntList(intList(n)(n)))(esUnlimited).get)
  log("chooser(unit(10))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + Ch07.Phase3.Par.chooser(Ch07.Phase3.Par.unit(10))(n => Ch07.Phase3.Par.sumIntList(intList(n)(n)))(esUnlimited).get)

  println("\n** Exercise 7.14 **")
  log("flatMap users \"inner\" join, i.e. to convert ppa:Par[Par[A]] to pa:Par[A] it calculates the inner Par[A] to A")
  log("flatMap(unit(2))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + Ch07.Phase3.Par.flatMap(Ch07.Phase3.Par.unit(2))(n => Ch07.Phase3.Par.sumIntList(intList(n)(n)))(esUnlimited).get)
  log("flatMap(unit(5))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + Ch07.Phase3.Par.flatMap(Ch07.Phase3.Par.unit(5))(n => Ch07.Phase3.Par.sumIntList(intList(n)(n)))(esUnlimited).get)
  log("flatMap(unit(10))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + Ch07.Phase3.Par.flatMap(Ch07.Phase3.Par.unit(10))(n => Ch07.Phase3.Par.sumIntList(intList(n)(n)))(esUnlimited).get)
  log("flatMap2 users \"outer\" join, i.e. to convert ppa:Par[Par[A]] to pa:Par[A] it calculates the outer Par[Par[A]] to Par[A]")
  log("flatMap2(unit(2))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + Ch07.Phase3.Par.flatMap2(Ch07.Phase3.Par.unit(2))(n => Ch07.Phase3.Par.sumIntList(intList(n)(n)))(esUnlimited).get)
  log("flatMap2(unit(5))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + Ch07.Phase3.Par.flatMap2(Ch07.Phase3.Par.unit(5))(n => Ch07.Phase3.Par.sumIntList(intList(n)(n)))(esUnlimited).get)
  log("flatMap2(unit(10))(n => sumIntList(intList(n)(n)))(esUnlimited).get = " + Ch07.Phase3.Par.flatMap2(Ch07.Phase3.Par.unit(10))(n => Ch07.Phase3.Par.sumIntList(intList(n)(n)))(esUnlimited).get+"\n")

  log("********************************************************************\n")
  log("*** Shutting down the executor services es1, es2 and esunlimited ***\n")
  shutExecService(es1)
  shutExecService(es2)
  shutExecService(esUnlimited)
}
