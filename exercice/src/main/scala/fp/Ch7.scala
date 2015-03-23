package fp

import java.util.concurrent.{TimeUnit}


object Ch7 {

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
    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = ???
    def fork[A](a: => Par[A]): Par[A] = ???
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
    def run[A](a: Par[A]): A = ???

    // 7.2 Before continuing, try to come up with representations for Par that make it possible
    // to implement the functions of our API.
  }
































  object Phase3 {

    type Par[A] = ExecutorService => Future[A]

    class ExecutorService {
      def submit[A](a: Callable[A]): Future[A] = ???
    }

    trait Callable[A] {
      def call: A
    }

    trait Future[A] {
      def get: A
      def get(timeout: Long, unit: TimeUnit): A
      def cancel(evenIfRunning: Boolean): Boolean
      def isDone: Boolean
      def isCancelled: Boolean
    }

    object Par {
      def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

      private case class UnitFuture[A](get: A) extends Future[A] {
        def isDone = true
        def get(timeout: Long, units: TimeUnit) = get
        def isCancelled = false
        def cancel(evenIfRunning: Boolean): Boolean = false
      }

      def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

      def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
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


      // 7.4 This API already enables a rich set of operations. Here’s a simple example: using lazyUnit,
      // write a function to convert any function A => B to one that evaluates its result asynchronously.

      def asyncF[A,B](f: A => B): A => Par[B] = ???

      // 7.5 Hard: Write this function, called sequence. No additional primitives are required. Do not call run.
      def sequence[A](ps: List[Par[A]]): Par[List[A]] = ???


    }

  }
}
