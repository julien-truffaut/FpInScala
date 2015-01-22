import scala.annotation.tailrec

object nith_Chapter_02{

  val f = (i:Int,j:Int)=>i+j

  def compose[A,B,C](f:B=>C, g:A=>B):A=>C = (a:A) => f(g(a))

  def curry[A,B,C](f: (A,B) => C): A => (B => C) = (a:A) => (b:B) => f(a,b):C

  def unCurry[A,B,C](f: A => (B => C)): (A,B) => C = (a:A,b:B) => f(a)(b):C

  def fibonacci(n: Int): BigInt = {
    @tailrec
    def go(i: Int,x:BigInt,y:BigInt):BigInt = {
      if (i<2) x
      else go(i-1,y,x+y)
    }
    go(n,0,1)
  }

  def isSorted[A](as: Array[A], isOrdered: (A,A) => Boolean): Boolean = {
    @tailrec
    def go(i: Int): Boolean = { (i<1) || (isOrdered(as(i-1),as(i)) && go(i-1)) }
    go(as.size-1)
  }



  def main(args: Array[String]) {
    val integerArgs:Array[Int] = args.map((s:String) => s.toInt)

    println("****** Chapter_02 ******")
    println("************************")
    println("*** Testing isSorted ***")
    assert(isSorted(Array(),(i:Int,j:Int) => (i<j):Boolean))
    assert(isSorted(Array(0),(i:Int,j:Int) => (i<j):Boolean))
    assert(isSorted(Array(0,1,2,4),(i:Int,j:Int) => (i<j):Boolean))
    assert(!isSorted(Array(0,0,1,2,4),(i:Int,j:Int) => (i<j):Boolean))
    println("Tach! " + " Thanks for giving me "+ args.size +" arguments.")
    println("The arguments are sorted: " + isSorted(integerArgs,(i:Int,j:Int) => (i<j):Boolean))
    println("*** Fibonacci numbers ***")
    integerArgs.foreach((i: Int) => println("fibonacci(" + i + ")=" + fibonacci(i)))
    println("*** Curry, UnCurry and Compose***")
    println("( (i:Int,j:Int)=>i+j )(2,3) = " + ( (i:Int,j:Int)=>i+j )(2,3))
    println("( curry( (i:Int,j:Int)=>i+j ) (2)(3) = " + curry( (i:Int,j:Int)=>i+j ) (2)(3))
    println("( unCurry(curry( (i:Int,j:Int)=>i+j )) (2,3) = " + unCurry(curry( (i:Int,j:Int)=>i+j )) (2,3))
    assert(compose((i:Int)=>i*2,(i:Int)=>i+3)(5) == 16)
    println("compose((i:Int)=>i*2,(i:Int)=>i+3)(5) = " + compose((i:Int)=>i*2,(i:Int)=>i+3)(5))
    println("compose(unCurry[Int, Int, Int],curry[Int, Int, Int])( (i:Int,j:Int)=>i+j )(3,4) = " + compose(unCurry[Int, Int, Int],curry[Int, Int, Int])( (i:Int,j:Int)=>i+j )(3,4))
    assert( unCurry(curry( (i:Int,j:Int)=>i+j )) (22,33) == ((i:Int,j:Int)=>i+j)(22,33) )
    assert( compose(unCurry[Int, Int, Int],curry[Int, Int, Int])( (i:Int,j:Int)=>i+j ) (22,33) == ((i:Int,j:Int)=>i+j)(22,33) )
  }
}
