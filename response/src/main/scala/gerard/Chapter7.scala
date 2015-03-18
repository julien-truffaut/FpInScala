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

    case class ExpiringFuture[A](f: Future[A], timeout: Long, units: TimeUnit)

    type Par[A] = ExecutorService => Future[A]

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit) = get

      def isCancelled = false

      def cancel(evenIfRunning: Boolean): Boolean = false
    }

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
  }

}
