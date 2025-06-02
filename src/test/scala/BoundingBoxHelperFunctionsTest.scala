package org.amizan

import org.specs2.mutable.Specification
import BoundingBoxHelperFunctions._
import BoundingBoxDataClasses.{Box, Point}

class BoundingBoxHelperFunctionsTest extends Specification {

  "BoundingBoxHelperFunctionsTest" should {
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
      "No points" >> {
        val lines = List("---")
        val points = Nil

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
      "a single point" >> {
        val points = List(Point(1,1))
        val box = buildBox(points)
        box must beEqualTo(Box(Point(1,1), Point(1,1)))
      }
      "multiple points" >> {
        val points = List(Point(2,2), Point(1,1))
        val box = buildBox(points)
        box must beEqualTo(Box(Point(1,1), Point(2,2)))
      }
    }

    "findAdjacentPointsInUnvisited" in {
      "Count an adjacent neighbor" >> {
        val point = Point(1,1)
        val unvisitedPoints = Set(Point(1,2), Point(3,3))
        val pointsToVisit = Set(Point(1,2))
        val remainingUnvisited = Set(Point(3,3))

        findAdjacentPointsInUnvisited(point, unvisitedPoints) must_== (pointsToVisit, remainingUnvisited)
      }
      "Not count diagonal neighbors" >> {
        val point = Point(1,1)
        val unvisitedPoints = Set(Point(2,2), Point(3,3))
        val pointsToVisit = Set()
        val remainingUnvisited = Set(Point(2,2), Point(3,3))

        findAdjacentPointsInUnvisited(point, unvisitedPoints) must_== (pointsToVisit, remainingUnvisited)
      }
      "Process if all neighbors are adjacent" >> {
        val point = Point(2,2)
        val unvisitedPoints = Set(Point(1,2), Point(2,1), Point(3,2), Point(2,3))
        val pointsToVisit = Set(Point(1,2), Point(2,1), Point(3,2), Point(2,3))
        val remainingUnvisited = Set()

        findAdjacentPointsInUnvisited(point, unvisitedPoints) must_== (pointsToVisit, remainingUnvisited)
      }
    }

    "findBoundingBoxes" in {
      "Count single points as boxes" >> {
        val points = List(Point(1,1), Point(2,2))
        val boxes = findBoundingBoxes(points)
        boxes must containTheSameElementsAs(List(Box(Point(1,1), Point(1,1)), Box(Point(2,2), Point(2,2))))
      }
      "Find L-shaped boxes" >> {
        val points = List(
          Point(6,6),

          Point(1,1),
          Point(2,1),
          Point(3,1),
          Point(4,1),
          Point(4,2),
          Point(4,3),
          Point(4,4),
        )

        val boxes = findBoundingBoxes(points)
        boxes must containTheSameElementsAs(List(Box(Point(1,1), Point(4,4)), Box(Point(6,6), Point(6,6))))
      }

      "Find completely contained boxes" >> {
        val points = List(
          Point(1, 1),
          Point(1, 2),
          Point(2, 1),
          Point(2, 2),

          Point(5, 5),
          Point(5, 6),
          Point(6, 5),
          Point(6, 6),
          Point(6, 7),
          Point(7, 7)
        )

        val boxes = findBoundingBoxes(points)
        boxes must containTheSameElementsAs(List(
          Box(Point(1, 1), Point(2, 2)),
          Box(Point(5, 5), Point(7, 7))
        ))
      }
    }

    "findNonOverlappingBoxes" in {
      "Return all boxes when none overlap" >> {
        val allBoxes = List(
          Box(Point(1, 1), Point(2, 2)),
          Box(Point(4, 4), Point(5, 5)),
          Box(Point(6, 6), Point(6, 6)), 
        )

        findNonOverlappingBoxes(allBoxes) must containTheSameElementsAs(allBoxes)
      }
      "Remove boxes that overlap" >> {
        val allBoxes = List(
          Box(Point(1, 1), Point(3, 3)),
          Box(Point(2, 2), Point(4, 4)),
          Box(Point(5, 5), Point(6, 6))
        )
        val expectedNonOverlapping = List(
          Box(Point(5, 5), Point(6, 6))
        )

        findNonOverlappingBoxes(allBoxes) must containTheSameElementsAs(expectedNonOverlapping)
      }
      "Return no boxes if all overlap" >> {
        val allBoxes = List(
          Box(Point(1, 1), Point(3, 3)),
          Box(Point(2, 2), Point(4, 4)),
          Box(Point(3, 3), Point(5, 5))
        )
        val expectedNonOverlapping = Nil

        findNonOverlappingBoxes(allBoxes) must containTheSameElementsAs(expectedNonOverlapping)
      }
    }
  }
}
