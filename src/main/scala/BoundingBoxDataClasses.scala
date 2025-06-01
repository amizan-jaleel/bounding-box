package org.amizan

object BoundingBoxDataClasses {
  final case class Point(x: Int, y: Int) {
    override def toString: String = s"($x,$y)"
  }

  final case class Box(topLeft: Point, bottomRight: Point) {
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
      points.map(other.contains).exists(identity)
    }
  }

}
