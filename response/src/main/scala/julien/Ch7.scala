package julien

import java.util.concurrent.{TimeUnit}


object Ch7 {

  object Phase1 {
    trait Par[A]

    def unit[A](a: => A): Par[A] = ???
    def get[A](a: Par[A]): A = ???

    get(unit(2 + 2)) == 4


    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      unit(f(get(a), get(b)))


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
    import java.util.concurrent._

    type Par[A] = ExecutorService => Future[A]

    object Par {
      def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

      private case class UnitFuture[A](get: A) extends Future[A] {
        def isDone = true
        def get(timeout: Long, units: TimeUnit) = get
        def isCancelled = false
        def cancel(evenIfRunning: Boolean): Boolean = false
      }

      def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a.apply(s)

      def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
        (es: ExecutorService) => {
          val af = a(es)
          val bf = b(es)
          UnitFuture(f(af.get, bf.get))
          new Future[C] {
            def get: C = f(af.get, bf.get)
            def get(timeout: Long, unit: TimeUnit): C =
              f(af.get(timeout, unit), bf.get(timeout, unit))
            def cancel(evenIfRunning: Boolean): Boolean = ???
            def isDone: Boolean = ???
            def isCancelled: Boolean = ???
          }
        }

      def fork[A](a: => Par[A]): Par[A] =
        es => es.submit(new Callable[A] {
          def call = a(es).get
        })
      def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
      def map[A,B](a: Par[A])(f: A => B): Par[B] = map2(a, unit(()))((a, _) => f(a))

      // 7.3 Hard: Fix the implementation of map2 so that it respects the contract of timeouts on Future.

      val five: Par[Int] = map2(unit(2), unit(3))(_ + _)

      // 7.4 This API already enables a rich set of operations. Here’s a simple example: using lazyUnit,
      // write a function to convert any function A => B to one that evaluates its result asynchronously.

      def asyncF[A,B](f: A => B): A => Par[B] =
        a => lazyUnit(f(a))

      // 7.5 Hard: Write this function, called sequence. No additional primitives are required. Do not call run.
      def sequence[A](ps: List[Par[A]]): Par[List[A]] =
        ps.foldLeft(unit(List[A]())){ (acc,elem) => map2(elem, acc)(_ :: _)  }


      def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =
        asyncF[List[A], List[A]](xs => xs.filter(f)).apply(as)

      def parMergeSort[A](as: List[A])(implicit ev: Ordering[A]): Par[List[A]] = {
        def merge(a: List[A],b: List[A]): List[A] = {
          def loop(l1: List[A],l2: List[A],res: List[A]): List[A] = (l1,l2) match {
            case (Nil,Nil) => res.reverse
            case (Nil, ys) => res.reverse ::: ys
            case (xs,Nil) => res.reverse ::: xs
            case (x :: xs, y :: ys) => if(ev.compare(x,y) < 0)
              loop(x :: xs,ys,y :: res)
            else
              loop(xs,y::ys,x::res)
          }
          loop(a,b,Nil)
        }
        // par stuff
        def parMerge(a: Par[List[A]],b: Par[List[A]]): Par[List[A]] = map2(a,b)(merge)
        val (left,right) = as.splitAt(as.size/2)
        parMerge(parMergeSort(left),parMergeSort(right))
      }

      def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
        es => map(cond)(b => if (b) t else f)(es).get()(es)


      def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
        es => {
          val _a: A = a(es).get()
          f(_a)(es)
        }

      def join[A](a: Par[Par[A]]): Par[A] = flatMap(a)(identity(_))


      class ParOps[A](a: Par[A]){
        def map[B](f: A => B): Par[B] = Par.map(a)(f)
      }

      new ParOps(Par.unit(9)).map(_ + 1)

      implicit def parOps[A](a: Par[A]): ParOps[A] = new ParOps(a)

      Par.unit(9).map(_ + 1)

    }



  }
}
