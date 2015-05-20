import java.util.Calendar

object util {
  final def addTimeStamp(str:String): String = Calendar.getInstance().getTime() + "  " + str

  final def log(x:Any) : Unit = println(addTimeStamp(x.toString))

  final def logException(exce:Exception)(x:Any):Unit = System.err.println(addTimeStamp(x.toString + " = " + exce))

}
