package org.amizan

object BoundingBoxDataClasses {
  final case class Point(x: Int, y: Int) {
    require(x >= 0 && y >= 0)
    override def toString: String = s"($x,$y)"
  }

  final case class Box(topLeft: Point, bottomRight: Point) {
    require(topLeft.x <= bottomRight.x && topLeft.y <= bottomRight.y)
    override def toString: String = s"$topLeft$bottomRight"
    private lazy val topRight: Point =
      Point(topLeft.x, bottomRight.y)
    private lazy val bottomLeft: Point =
      Point(bottomRight.x, topLeft.y)
      
    def contains(point: Point): Boolean = {
      point.x >= topLeft.x && point.x <= bottomRight.x &&
      point.y >= topLeft.y && point.y <= bottomRight.y
    }

    def overlaps(other: Box): Boolean = {
      val points = topLeft :: bottomRight :: topRight :: bottomLeft :: Nil
      val otherPoints = other.topLeft :: other.bottomRight :: other.topRight :: other.bottomLeft :: Nil
      // Return true if any of the other's corners are within this box
      points.map(other.contains).exists(identity) ||
        // We have to handle this edge case,
        // in case the one box is completely inside another one
        otherPoints.map(this.contains).exists(identity)
    }
  }

}
