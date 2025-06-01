package org.amizan

import org.specs2.mutable.Specification
import BoundingBoxHelperFunctions._
import BoundingBoxDataClasses.Point

class BoundingBoxHelperFunctionsTest extends Specification {

  "BoundingBoxHelperFunctionsTest" should {
    "findAdjacentPointsInUnvisited" in {
      ok
    }

    "toBool" in {
      "the valid input case" >> {
        var counter = 0
        def getInt(): Int = {
          counter += 1
          counter
        }
        toBool('*', getInt(), getInt()) must beTrue
        toBool('-', getInt(), getInt()) must beFalse
        counter must beEqualTo(0) // the function to get the int isn't called
      }

      "the invalid input case" >> {
        var counter = 0
        def getInt(): Int = {
          counter += 1
          counter
        }
        toBool('™', getInt(), getInt()) must throwA[IllegalArgumentException]
        counter must beEqualTo(2)
      }
    }

    "parseInput" in {
      "the valid input case" >> {
        val lines = List(
          "----",
          "-**-",
          "----"
        )
        val points = List(
          Point(2,2),
          Point(2,3),
        )

        parseInput(lines) must_== points
      }
      "minimum valid input" >> {
        val lines = List("*")
        val points = List(Point(1,1))

        parseInput(lines) must_== points
      }

      "Empty input" >> {
        val lines = Nil
        parseInput(lines) must throwA[IllegalArgumentException]("Empty input!")
      }

      "Irregular rows" >> {
        val lines = List(
          "----",
          "-**-",
          "----",
          "---"
        )
        parseInput(lines) must throwA[IllegalArgumentException]("Row 3 is not the same size as the other rows!")}

      "Bad character" >> {
        val lines = List(
          "----",
          "-**™",
          "----",
          "----",
        )

        parseInput(lines) must throwA[IllegalArgumentException]("Bad Input Character at: Row 1 Col 3: ™")
      }
    }

    "buildBox" in {
      ok
    }

    "findBoundingBoxes" in {
      ok
    }

    "findNonOverlappingBoxes" in {
      ok
    }
  }
}
