package julien

import java.util.concurrent.{TimeUnit, ThreadFactory, Executors, ExecutorService}

import org.specs2.ScalaCheck
import org.specs2.scalaz.Spec

import scala.util.Try
import scalaz.\/


class Ch7Spec extends Spec with ScalaCheck {

  val es: ExecutorService = {
    Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors, new ThreadFactory {
      val defaultThreadFactory = Executors.defaultThreadFactory()
      def newThread(r: Runnable) = {
        val t = defaultThreadFactory.newThread(r)
        t.setDaemon(true)
        t
      }
    })
  }

  "Par Phase 3" should {
    import julien.Ch7.Phase3._

    "run(map(unit(x))(id)) == x" in prop { x: Int =>
      Par.run(es)(
        Par.map(Par.unit(x))(identity)
      ).get()  == x
    }

    "deadlock" in {
      val S = Executors.newFixedThreadPool(1)
      \/.fromTryCatchNonFatal(7)
      Try(Par.fork(Par.lazyUnit(1))(S).get(2, TimeUnit.SECONDS)).isFailure ==== true
    }

  }
  

}
