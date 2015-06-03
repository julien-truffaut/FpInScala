import java.util.Calendar

object util {
  final def addTimeStamp(str:String): String = Calendar.getInstance().getTime() + "  " + str

  final def log(x:Any) : Unit = println(addTimeStamp(x.toString))

  final def logException(exce:Exception)(x:Any):Unit = System.err.println(addTimeStamp(x.toString + " = " + exce))

  // mod: positive remainder of division, i.e. mod(-3)(4) = 1 and mod(3)(4) = 3
  final def mod(n:Int)(m:Int):Int = ((n%m)+m)%m
}
