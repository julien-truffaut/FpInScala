import java.util.Calendar
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ThreadFactory, Executors, TimeUnit, ExecutorService}

object util {
  final def addTimeStamp(str: String): String = Calendar.getInstance().getTime() + "  " + str

  def log(x: Any): Unit = println(addTimeStamp(x.toString))

  def logg(s: String)(x: Any): Unit = println(addTimeStamp(s + "\t= " + x.toString))

  final def logException(exce: Exception)(x: Any): Unit = System.err.println(addTimeStamp(x.toString + " = " + exce))

  // mod: positive remainder of division, i.e. mod(-3)(4) = 1 and mod(3)(4) = 3
  final def mod(n: Int)(m: Int): Int = ((n % m) + m) % m

  // some stuff for the executors
  final def esUnlimited: ExecutorService = Executors.newCachedThreadPool(new ThreadFactory {
    val counter = new AtomicInteger(0)

    def newThread(r: Runnable): Thread = {
      val t = new Thread(r, s"PAR-thread-${counter.getAndIncrement}")
      t.setDaemon(true)
      t
    }
  })


  final def shutExecService(debug: Boolean = false)(execService: ExecutorService): Unit = {
    if (debug) {
      logg("Shutting down now executor service")(execService.toString)
      logg(execService.toString + ".shutdown()")(execService.shutdown())
      logg(execService.toString + ".shutdownNow()")(execService.shutdownNow())
      if (!execService.awaitTermination(8, TimeUnit.SECONDS))
        logException(new Exception)("Executor Service did not terminate \n" + execService.toString)
      else logg("Executor terminated.")(execService.toString)
    }
    else {
      execService.shutdown()
      execService.shutdownNow()
    }
  }
}
