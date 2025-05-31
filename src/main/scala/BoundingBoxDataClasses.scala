package org.amizan

object BoundingBoxDataClasses {
  final case class Point(x: Int, y: Int) {
    override def equals(obj: Any): Boolean = obj match {
      case Point(x, y) => x == this.x && y == this.y
      case _ => false
    }
    override def toString: String = s"($x,$y)"
    lazy val plusOne: Point =
      Point(x+1, y+1)
  }

  final case class Box(topLeft: Point, bottomRight: Point) {
    override def equals(obj: Any): Boolean = obj match {
      case Box(topLeft, bottomRight) =>
        topLeft == this.topLeft && bottomRight == this.bottomRight
      case _ => false
    }
    override def toString: String = s"$topLeft$bottomRight"
    lazy val plusOne: Box =
      Box(topLeft.plusOne, bottomRight.plusOne)
    lazy val topRight: Point =
      Point(topLeft.x, bottomRight.y)
    lazy val bottomLeft: Point =
      Point(bottomRight.x, topLeft.y)
      
    def contains(point: Point): Boolean = {
      point.x >= topLeft.x && point.x <= bottomRight.x &&
      point.y >= topLeft.y && point.y <= bottomRight.y
    }

    def overlaps(other: Box): Boolean = {
      if(this == other) {
        // if the two boxes are equal, they don't 'overlap', they are the same
        false
      }
      else {
        val points = topLeft :: bottomRight :: topRight :: bottomLeft :: Nil
        points.map(other.contains).exists(identity)
      }
    }
  }

}
