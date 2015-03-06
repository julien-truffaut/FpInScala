import scala.annotation.tailrec

sealed trait List[+A]

//case object Nil extends List[Nothing]
//
//case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  def apply[A](as: A*): List[A] = {
    @tailrec
    def go(l: List[A], as: A*): List[A] =
      if (as.isEmpty) l
      else go(Cons(as.head, l), as.tail: _*)
    go(Nil, as.reverse: _*)
  }


  //exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, t) => t
  }

  //exercise 3.3
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(a, t)
  }

  //exercise 3.4
  @tailrec
  def drop[A](as: List[A], n: Int): List[A] = as match {
    case Nil => Nil
    case _ if n < 1 => as
    case Cons(x, t) => drop(t, n - 1)
  }

  //exercise 3.5
  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, t) if f(x) => dropWhile(t)(f)
    case _ => l
  }

  //exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, t) => Cons(x, init(t))
  }

  //exercise 3.7
  def foldRight[A, B](as: List[A], z: B)(f: A => (=> B) => B): B = {
  //  println("foldRight(%s)".format(as + "," + z))
    as match {
      case Nil => z
      case Cons(x, xs) => f(x)(foldRight(xs, z)(f))
    }
  }

  def sum(l: List[Int]): Int = {
    @tailrec
    def go(l: List[Int], n: Int): Int = l match {
      case Nil => n
      case Cons(x, xs) => go(xs, x + n)
    }
    go(l, 0)
  }

  def product(l: List[Int]): Int = {
    @tailrec
    def go(l: List[Int], n: Int): Int = l match {
      case Nil => n
      case Cons(x, xs) => go(xs, x * n)
    }
    go(l, 1)
  }

  def sumFoldRight(l: List[Int]) = foldRight(l, 0)(x => y => x + y)

  def productFoldRight(l: List[Int]) = foldRight(l, 1)(x => if (x == 0) _ => 0 else y => x * y)

  // end of exercise 3.7

  //exercise 3.10
  // first B parameter B is meant to be referenced by name instead of value
  // for lazy evaluation, so (=>B) instead of B
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: A => (=> B) => B): B = {
    // println("foldLeft(%s)".format(as + "," + z))
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(x)(z))(f)
    }
  }

  //exercise 3.11
  def sumFoldLeft(l: List[Int]) = foldLeft(l, 0)(x => y => x + y)

  def productFoldLeft(l: List[Int]) = foldLeft(l, 1)(x => if (x == 0) _ => 0 else y => x * y)

  def length[A](as: List[A]): Int = foldLeft(as, 0)(a => y => 1 + y)

  //exercise 3.12
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])(a => l => Cons(a, l))

  //exercise 3.14
  def append[A](l1: List[A], l2: List[A]): List[A] = foldLeft(reverse(l1), l2)(a => as => Cons(a, as))

  //exercise 3.15
  def concat[A](ass: List[List[A]]): List[A] = ass match {
    case Nil => Nil
    case Cons(h, t) => append(h, concat(t))
  }

  //exercise 3.17
  def myString[A](as: List[A]): String = {
    @tailrec
    def go(as: List[A], s: String): String = {
      val sAppend: String = if (s == "") "" else s + ","
      as match {
        case Nil => sAppend
        case Cons(h, Nil) => sAppend + h.toString
        case Cons(h, t) => go(t, sAppend + h.toString)
      }
    }
    "List(" + go(as, "") + ")"
  }

  //exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = reverse(foldLeft[A, List[B]](as, Nil)(a => bs => Cons(f(a), bs)))

  //exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = reverse(foldLeft[A, List[A]](as, Nil)(a => l => if (f(a)) Cons(a, l) else l))

  //exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = reverse(foldLeft[A, List[B]](as, Nil)(a => bs => append(f(a), bs)))

  //exercise 3.21
  def flatFilter[A](as: List[A])(f: A => Boolean): List[A] = flatMap[A, A](as)(a => if (f(a)) List(a) else Nil)

  //exercise 3.23: Pair and zipWith
  def Pair[A, B](as: List[A])(bs: List[B]): List[(A, B)] = (as, bs) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(a, ta), Cons(b, tb)) => Cons((a, b), Pair(ta)(tb))
  }

  def zipWith[A, B, C](as: List[A])(bs: List[B])(f: A => B => C): List[C] =
    reverse(foldLeft[Tuple2[A, B], List[C]](List.Pair(as)(bs), Nil)(x => l => Cons((f(x._1)(x._2)), l)))

  //exercise 3.24: isInitialSegment and hasSubsequence
  @tailrec
  def isInitialSegment[A](ini: List[A], as: List[A]): Boolean = ini match {
    case Nil => true
    case Cons(i, iniTail) => as match {
      case Nil => false
      case Cons(h, t) if i == h => isInitialSegment(iniTail, t)
      case _ => false
    }
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sub match {
    case Nil => true
    case Cons(a, as) => List.dropWhile(sup)(x => x != a) match {
      case Nil => false
      case Cons(a1, Nil) => as == Nil
      case Cons(a1, Cons(a2, t)) => isInitialSegment(as, Cons(a2, t)) || hasSubsequence(Cons(a2, t), sub)
    }
  }


  final def exists[A](as:List[A])(p: A => Boolean): Boolean = as match {
    case Cons(h, t) => p(h) || exists(t)(p)
    case _ => false
  }

}

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def fold[A, B](as: Tree[A])(g: A => B)(f: (=> B) => (=> B) => B): B = as match {
    case Leaf(a) => g(a)
    case Branch(l, r) => f(fold(l)(g)(f))(fold(r)(g)(f))
  }

  //exercise 3.25
  def size[A](as: Tree[A]): Int = as match {
    case Leaf(a) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def sizeFold[A](as: Tree[A]): Int = fold(as)(a => 1)(x => y => x + y + 1)

  //exercise 3.26
  def maxVal(as: Tree[Int]): Int = fold[Int, Int](as)(i => i)(x => y => x.max(y))

  //exercise 3.27
  def depth[A](as: Tree[A]): Int = fold[A, Int](as)(i => 0)(x => y => 1 + x.max(y))

  //exercise 3.28
  def map[A, B](as: Tree[A])(h: A => B): Tree[B] = fold[A, Tree[B]](as)(a => Leaf(h(a)))(l => r => Branch(l, r))

  // stringLength transforms a tree of strings into a tree containing the length of each string
  def stringLength(strings: Tree[String]): Tree[Int] = map[String, Int](strings)(s => s.length)
}


sealed trait FinTree[+A]

case class fTree[A](value: A, ts: List[FinTree[A]]) extends FinTree[A]

object FinTree {

  import List.{Nil,Cons}

  def branches[A](t: FinTree[A]): List[FinTree[A]] = t match {
    case fTree(value, ts) => ts
  }

  def rootValue[A](t: FinTree[A]): A = t match {
    case fTree(value, ts) => value
  }

  //exercise 3.25
  def size[A](t: FinTree[A]): Int = List.foldLeft[FinTree[A], Int](FinTree.branches(t), 1)(x => i => i + FinTree.size[A](x))

  //exercise 3.26
  def maxVal(t: FinTree[Int]): Int = List.foldLeft[FinTree[Int], Int](FinTree.branches(t), FinTree.rootValue(t))(x => i => i.max(FinTree.maxVal(x)))

  //exercise 3.27
  def depth[A](t: FinTree[A]): Int = List.foldLeft[FinTree[A], Int](FinTree.branches(t), 0)(x => i => i.max(1 + FinTree.depth[A](x)))

  //exercise 3.28
  def map[A, B](t: FinTree[A])(h: A => B): FinTree[B] =
    List.foldLeft[FinTree[A], FinTree[B]](FinTree.branches(t), fTree(h(FinTree.rootValue(t)), Nil))(at => bt => fTree(FinTree.rootValue(bt), List.reverse(Cons(FinTree.map(at)(h), FinTree.branches(bt)))))

  //  List.foldLeft[FinTree[A],FinTree[B]](FinTree.branches(t),fTree(h(FinTree.rootValue(t)),Nil))(at=>bt=>fTree(h(FinTree.rootValue(at)),Cons(FinTree.map(at)(h),FinTree.branches(bt))))
  def stringLength(strings: FinTree[String]): FinTree[Int] = map[String, Int](strings)(s => s.length)
}


object nith_Chapter_03 {

  import List.{Nil,Cons}

  //exercise 3.1
  def matchFun(l: List[Int]): Int = l match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    //    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.product(t)
    case _ => 101
  }

  def main(args: Array[String]) {

    val nilList: List[Double] = Nil
    val oneList: List[Int] = Cons(1, Nil)
    val strList: List[String] = Cons("a", Cons("b", Nil))
    val dblList: List[Double] = List(0.1, 1.2, 2.3, 3.4, 4.5)
    val fivList: List[Int] = List(0, 1, 2, 3, 4)
    val tenList: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    val hunList: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29
      , 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59
      , 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89
      , 90, 91, 92, 93, 94, 95, 96, 97, 98, 99)

    println("****** Chapter_03 ******")
    println("************************")

    println("****** a few lists ******")
    println("nilList=" + List.myString(nilList))
    println("oneList=" + List.myString(oneList))
    println("strList=" + List.myString(strList))
    println("dblList=" + List.myString(dblList))
    println("fivList=" + List.myString(fivList))
    println("tenList=" + List.myString(tenList))
    println("hunList=" + List.myString(hunList))
    println("****** playing with fivList ******")
    println("matchFun(fivList)=" + matchFun(fivList))
    println("tail(fivList)=" + List.tail(fivList))
    println("setHead(fivList,42)=" + List.setHead(fivList, 42))
    println("drop(fivList,-1)=" + List.drop(fivList, -1))
    println("drop(fivList,0)=" + List.drop(fivList, 0))
    println("drop(fivList,3)=" + List.drop(fivList, 3))
    println("drop(fivList,9)=" + List.drop(fivList, 9))
    println("dropWhile(fivList)(i=>(i==i))=" + List.dropWhile(fivList)(i => i == i))
    println("dropWhile(fivList)(i=>(i<4))=" + List.dropWhile(fivList)(i => i < 4))
    println("dropWhile(fivList)(i=>(i%3<2))=" + List.dropWhile(fivList)(i => i % 3 < 2))
    println("****** method init ******")
    println("init(oneList)=" + List.init(oneList))
    println("init(strList)=" + List.init(strList))
    println("init(fivList)=" + List.init(fivList))
    println("****** a few lists ******")
    println("nilList=" + List.myString(nilList))
    println("oneList=" + List.myString(oneList))
    println("strList=" + List.myString(strList))
    println("dblList=" + List.myString(dblList))
    println("fivList=" + List.myString(fivList))
    println("tenList=" + List.myString(tenList))
    println("hunList=" + List.myString(hunList))
    println("****** foldRight and foldLeft******")
    println("sum(fivList)=" + List.product(fivList))
    println("sum(tenList)=" + List.product(tenList))
    println("sum(hunList)=" + List.product(hunList))
    println("sumFoldRight(tenList)=" + List.sumFoldRight(tenList))
    println("sumFoldLeft(tenList)=" + List.sumFoldLeft(tenList))
    println("product(List.tail(fivList))=" + List.product(List.tail(fivList)))
    println("productFoldRight(tenList)=" + List.productFoldRight(tenList))
    println("productFoldLeft(tenList)=" + List.productFoldLeft(tenList))
    println("foldRight(fivList, Nil:List[Int])(a=>as=>Cons(a,as))))=" + List.foldRight(fivList, Nil: List[Int])(a => as => Cons(a, as)))
    println("length(fivList)=" + List.length(fivList))
    println("length(tenList)=" + List.length(tenList))
    println("length(hunList)=" + List.length(hunList))
    println("sumFoldRight(hunList)=" + List.sumFoldRight(hunList))
    println("sumFoldLeft(hunList)=" + List.sumFoldLeft(hunList))
    println("product(hunList)=" + List.product(hunList))
    println("productFoldRight(hunList)=" + List.productFoldRight(hunList))
    println("productFoldLeft(hunList)=" + List.productFoldLeft(hunList))
    println("reverse(tenList)=" + List.myString(List.reverse(tenList)))
    println("append(fivList,tenList)=" + List.myString(List.append(fivList, tenList)))
    println("concat(fivList,strList,tenList)=" + List.myString(List.concat(List(fivList, strList, tenList))))
    println("map(fivList)(n=>n+1)=" + List.myString(List.map(fivList)(n => n + 1)))
    println("map(dblList)(toString)=" + List.myString(List.map(dblList)(d => d.toString)))
    println("flatMap(fivList)(n=>List(n+1))=" + List.myString(List.flatMap(fivList)(n => List(n + 1))))
    println("flatMap(fivList)(n=>List(n*n))=" + List.myString(List.flatMap(fivList)(n => List(n * n))))
    println("flatMap(fivList)(i => List(i,i)))=" + List.myString(List.flatMap(fivList)(i => List(i, i))))
    println("filter[Int](Nil)(n=>0==(n%2)=" + List.myString(List.filter[Int](Nil)(n => 0 == (n % 2))))
    println("filter(tenList)(n=>0==(n%2)=" + List.myString(List.filter(tenList)(n => 0 == (n % 2))))
    println("filter(tenList)(n=>0==(n%7)=" + List.myString(List.filter(tenList)(n => 0 == (n % 7))))
    println("flatFilter[Int](Nil)(n=>0==(n%2)=" + List.myString(List.flatFilter[Int](Nil)(n => 0 == (n % 2))))
    println("flatFilter(tenList)(n=>0==(n%2)=" + List.myString(List.flatFilter(tenList)(n => 0 == (n % 2))))
    println("flatFilter(tenList)(n=>0==(n%7)=" + List.myString(List.flatFilter(tenList)(n => 0 == (n % 7))))
    println("Pair(fivList)(tenList)(n=>m=>n+m)=" + List.myString(List.Pair(fivList)(tenList)))
    println("zipWith(fivList)(tenList)(n=>m=>n+m)=" + List.myString(List.zipWith(fivList)(tenList)(n => m => n + m)))
    println("zipWith(fivList)(tenList)(n=>m=>n*m)=" + List.myString(List.zipWith(fivList)(tenList)(n => m => n * m)))
    println("fivList=" + List.myString(fivList))
    println("tenList=" + List.myString(tenList))
    println("isInitialSegment(fivList,tenList,true)=" + List.isInitialSegment(fivList, tenList))
    println("isInitialSegment(tenList,fivList,true)=" + List.isInitialSegment(tenList, fivList))
    println("hasSubsequence(fivList,tenList)=" + List.hasSubsequence(fivList, tenList))
    println("hasSubsequence(tenList,fivList)=" + List.hasSubsequence(tenList, fivList))
    println("hasSubsequence(hunList,fivList)=" + List.hasSubsequence(hunList, fivList))
    println("hasSubsequence(hunList,tenList)=" + List.hasSubsequence(hunList, tenList))
    println("hasSubsequence(List(0,42,1,2,3,4),fivList)=" + List.hasSubsequence(List(0, 42, 1, 2, 3, 4), fivList))
    println("hasSubsequence(List(0,42,0,1,2,3,4),fivList)=" + List.hasSubsequence(List(0, 42, 0, 1, 2, 3, 4), fivList))
    println("****** Trees ******")
    println("size(Leaf(42))=" + Tree.size(Leaf(42)))
    println("size(Branch(Leaf(-1),Leaf(42)))=" + Tree.size(Branch(Leaf(-1), Leaf(42))))
    println("size(Branch(Branch(Leaf(-1),Leaf(0)),Leaf(42)))=" + Tree.size(Branch(Branch(Leaf(-1), Leaf(0)), Leaf(42))))
    println("sizeFold(Leaf(42))=" + Tree.sizeFold(Leaf(42)))
    println("sizeFold(Branch(Leaf(-1),Leaf(42)))=" + Tree.sizeFold(Branch(Leaf(-1), Leaf(42))))
    println("sizeFold(Branch(Branch(Leaf(-1),Leaf(0)),Leaf(42)))=" + Tree.sizeFold(Branch(Branch(Leaf(-100), Leaf(0)), Leaf(42))))
    println("maxVal(Leaf(42))=" + Tree.maxVal(Leaf(42)))
    println("maxVal(Branch(Leaf(-1),Leaf(42)))=" + Tree.maxVal(Branch(Leaf(-1), Leaf(42))))
    println("maxVal(Branch(Branch(Leaf(-1),Leaf(0)),Leaf(42)))=" + Tree.maxVal(Branch(Branch(Leaf(-100), Leaf(0)), Leaf(42))))
    println("depth(Leaf(42))=" + Tree.depth(Leaf(42)))
    println("depth(Branch(Leaf(-1),Leaf(42)))=" + Tree.depth(Branch(Leaf(-1), Leaf(42))))
    println("depth(Branch(Branch(Leaf(-1),Leaf(0)),Leaf(42)))=" + Tree.depth(Branch(Branch(Leaf(-100), Leaf(0)), Leaf(42))))
    println("stringLength(Leaf(a))=" + Tree.stringLength(Leaf("a")))
    println("stringLength(Branch(Leaf(a),Leaf(abc)))=" + Tree.stringLength(Branch(Leaf("a"), Leaf("abc"))))
    println("stringLength(Branch(Branch(Leaf(abc),Leaf()),Leaf(abcd)))="
      + Tree.stringLength(Branch(Branch(Leaf("abc"), Leaf("")), Leaf("abcd"))))
    println("****** Finitary branching trees with values at all nodes: FinTree ******")
    println("size(fTree(42,Nil))=" + FinTree.size(fTree(42, Nil)))
    println("size(fTree(123,List(fTree(-1,Nil),fTree(42,Nil)))))=" + FinTree.size(fTree(123, List(fTree(-1, Nil), fTree(42, Nil)))))
    println("size(fTree(456,List(fTree(123,List(fTree(-1,Nil),fTree(0,Nil))),fTree(42,Nil))))="
      + FinTree.size(fTree(456, List(fTree(123, List(fTree(-1, Nil), fTree(0, Nil))), fTree(42, Nil)))))
    println("maxVal(fTree(42,Nil))=" + FinTree.maxVal(fTree(42, Nil)))
    println("maxVal(fTree(123,List(fTree(-1,Nil),fTree(42,Nil)))))=" + FinTree.maxVal(fTree(123, List(fTree(-1, Nil), fTree(42, Nil)))))
    println("maxVal(fTree(456,List(fTree(123,List(fTree(-1,Nil),fTree(0,Nil))),fTree(42,Nil))))="
      + FinTree.maxVal(fTree(456, List(fTree(123, List(fTree(-1, Nil), fTree(0, Nil))), fTree(42, Nil)))))
    println("depth(fTree(42,Nil))=" + FinTree.depth(fTree(42, Nil)))
    println("depth(fTree(123,List(fTree(-1,Nil),fTree(42,Nil)))))=" + FinTree.depth(fTree(123, List(fTree(-1, Nil), fTree(42, Nil)))))
    println("depth(fTree(456,List(fTree(123,List(fTree(-1,Nil),fTree(0,Nil))),fTree(42,Nil))))="
      + FinTree.depth(fTree(456, List(fTree(123, List(fTree(-1, Nil), fTree(0, Nil))), fTree(42, Nil)))))
    println("depth(fTree(456,List(fTree(123,List(fTree(-1,List(fTree(7,Nil))),fTree(0,Nil))),fTree(42,List(fTree(7,Nil))))))="
      + FinTree.depth(fTree(456, List(fTree(123, List(fTree(-1, List(fTree(7, Nil))), fTree(0, Nil))), fTree(42, List(fTree(7, Nil)))))))
    println("****** string length for finTrees ******")
    println("stringLength(fTree(,Nil))=" + FinTree.stringLength(fTree("", Nil)))
    println("stringLength(fTree(abc,Nil))=" + FinTree.stringLength(fTree("abc", Nil)))
    println("stringLength(fTree(123,Nil))=" + FinTree.stringLength(fTree("123", Nil)))
    println("stringLength(fTree(123,List(fTree(,Nil))))=" + FinTree.stringLength(fTree("123", List(fTree("", Nil)))))
    println("stringLength(fTree(abc,List(fTree(abc,Nil))))=" + FinTree.stringLength(fTree("abc", List(fTree("abc", Nil)))))
    println("stringLength(fTree(123,List(fTree(ab,Nil)))))=" + FinTree.stringLength(fTree("123", List(fTree("ab", Nil)))))
    println("stringLength(fTree(123,List(fTree(a,Nil),fTree(ab,Nil)))))=" + FinTree.stringLength(fTree("123", List(fTree("a", Nil), fTree("ab", Nil)))))
    println("stringLength(fTree(456,List(fTree(123,List(fTree(ab,Nil),fTree(X,Nil))),fTree(abcdef,Nil))))="
      + FinTree.stringLength(fTree("456", List(fTree("123", List(fTree("ab", Nil), fTree("X", Nil))), fTree("abcdef", Nil)))))
    println("stringLength(fTree(456,List(fTree(123,List(fTree(ab,List(fTree(7,Nil))),fTree(X,Nil))),fTree(abcdef,List(fTree(7,Nil))))))="
      + FinTree.stringLength(fTree("456", List(fTree("123", List(fTree("ab", List(fTree("7", Nil))), fTree("X", Nil))), fTree("abcdef", List(fTree("7", Nil)))))))
  }
}
