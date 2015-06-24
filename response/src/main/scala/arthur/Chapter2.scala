package arthur

object Chapter2 {

  // 2.1 with tailrec
  def fib(n: Int): Int = {
    def loop(prev:Int, curr:Int, count:Int):Int = {
      if (count > 0) loop(curr, curr + prev, count - 1)
      else curr + prev
    }
    if (n == 0 || n == 1)  n
    else loop(0, 1, n)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop[A](as:Array[A], i:Int):Boolean = {
      if (i == as.length) true
      else loop(as, i + 1)
    }
    loop(as, 0)
  }

  // 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)


  // 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a,b) => f(a)(b)


  // 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}
