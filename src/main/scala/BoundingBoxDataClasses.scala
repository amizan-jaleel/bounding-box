package org.amizan

object BoundingBoxDataClasses {
  case class Point(x: Int, y: Int) {
    override def toString: String = s"($x,$y)"
    lazy val plusOne: Point =
      Point(x+1, y+1)
  }

  case class Box(topLeft: Point, bottomRight: Point) {
    override def toString: String = s"$topLeft$bottomRight"
    lazy val plusOne: Box =
      Box(topLeft.plusOne, bottomRight.plusOne)
  }

}
