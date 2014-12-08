package fp

import org.specs2.ScalaCheck
import org.specs2.scalaz.Spec
import scalaz.syntax.equal._

class Chapter2Spec extends Spec with ScalaCheck {


  "Chapter2" should {

    "prop1" in prop{ (xs: List[Int]) =>
      xs === xs
    }


  }

}
