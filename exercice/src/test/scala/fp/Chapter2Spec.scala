package fp

import org.scalatest.FreeSpec
import org.scalatest.prop.Checkers

class Chapter2Spec extends FreeSpec with Checkers {

  "Chapter2" - {

    "prop1" in check{ (xs: List[Int]) =>
      xs === xs
    }

  }

}
