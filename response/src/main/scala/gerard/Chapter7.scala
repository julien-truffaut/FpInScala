package gerard

import java.util.concurrent.{TimeUnit, Future, Callable, ExecutorService}

object `Chapter7.1` {

  case class Par[A]()

  object Par {
    def unit[A](a: => A): Par[A] = ???

    def get[A](a: Par[A]): A = ???

    def map2[A, B, C](ra: => Par[A], rb: => Par[B])(f: (A, B) => C): Par[C] = ???
  }

}

object `Chapter7.2` {

  type ThreadPool = Int

  case class Par[A](f: (ThreadPool) => (A, ThreadPool))

  object Par {
    def unit[A](a: A): Par[A] = ???

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

    def fork[A](a: => Par[A]): Par[A] = ???

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](a: Par[A]): A = ???
  }

}

object Chapter7 {

  object Par {
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    /** decorated future in order to pass timeout through */
    case class ExpiringFuture[A](f: Future[A], timeout: Long, units: TimeUnit) extends Future[A] {
      override def cancel(mayInterruptIfRunning: Boolean): Boolean = f.cancel(mayInterruptIfRunning)

      override def isCancelled: Boolean = f.isCancelled

      override def get(): A = f.get()

      override def get(timeout: Long, unit: TimeUnit): A = f.get(timeout, unit)

      override def isDone: Boolean = f.isDone
    }

    type Par[A] = ExecutorService => Future[A]

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit) = get

      def isCancelled = false

      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    /**
     * From the book:
     *
     * In order to respect timeouts,
     * we’d need a new Future implementation that
     * records the amount of time spent evaluating
     * af, and then subtracts that time from the
     * available time allocated for evaluating bf.
     *
     * I really don't understand that definition of timeout.
     * That means you specify a total amount of time that
     * is allowed for a calculation if the runtime of all
     * threads is added. I see that this is a definition that is
     * not depending on how many cores you have but... it's not what
     * the user experiences. I want to say: be done in 10 minutes,
     * otherwise stop. As a user I really don't care how many cores are used,
     * I only care about the time I have to wait, nothing else...
     */
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        val aResult = af.get
        val bResult = bf.get
        UnitFuture(f(aResult, bResult))
      }

    /**
     * This is the implementation from the official solution.
     * I would never have guessed that you should implement
     * the map function _in_ the future.
     * However I could also not see how a custom future implementation
     * would help with the timeout...
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
        case None    =>
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


    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        def call = a(es).get
      })
  }

}
