package org.amizan

import org.specs2.mutable.Specification
import BoundingBoxDataClasses.{Box, Point}

class BoundingBoxDataClassesTest extends Specification {
  "Point" should {
    "Throw an error for invalid dimensions" in {
      Point(-1,-1) must throwA[IllegalArgumentException]
    }
  }

  "Bounding Box" should {
    "be valid even of minimum size" in {
      Box(Point(1,1), Point(1,1)) must not(throwA[IllegalArgumentException])
    }

    "be invalid for the points are not in the right place" in {
      Box(Point(2,2), Point(1,1)) must throwA[IllegalArgumentException]
    }

    "determine if a Point is inside a Box or not" in {
      val box = Box(Point(1,1), Point(3,3))
      box.contains(Point(2,2)) must beTrue
      box.contains(Point(4,4)) must beFalse
    }

    "determine if another box is overlapping" in {
      val box1 = Box(Point(1,1), Point(3,3))
      val box2 = Box(Point(2,2), Point(6,6))
      val box3 = Box(Point(4,4), Point(5,5))
      box1.overlaps(box2) must beTrue
      box2.overlaps(box1) must beTrue
      box1.overlaps(box3) must beFalse
      box3.overlaps(box1) must beFalse

      // Case where one box is inside another one
      box2.overlaps(box3) must beTrue
      box3.overlaps(box2) must beTrue
    }

  }
}
