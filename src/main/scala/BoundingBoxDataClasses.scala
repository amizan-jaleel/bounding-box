package org.amizan

object BoundingBoxDataClasses {
  /**
   * A class representing a '*' from the input file
   *
   * @param x the x-coordinate of the point, 1-indexed
   * @param y the y-coordinate of the point, 1-indexed
   * @throws IllegalArgumentException if x or y is negative
   */
  final case class Point(x: Int, y: Int) {
    // Allow 0 just for ease of use during the DFS
    require(x >= 0 && y >= 0)
    override def toString: String = s"($x,$y)"
  }

  /**
   * A bounding box defined by its top-left and bottom-right corners.
   *
   * @param topLeft     The top-left corner of the box.
   * @param bottomRight The bottom-right corner of the box.
   * @throws IllegalArgumentException if the top left isn't actually above and to the left
   */
  final case class Box(topLeft: Point, bottomRight: Point) {
    // Allow them to be equal, as a single point is a valid box
    require(topLeft.x <= bottomRight.x && topLeft.y <= bottomRight.y)
    override def toString: String = s"$topLeft$bottomRight"
    private lazy val topRight: Point =
      Point(topLeft.x, bottomRight.y)
    private lazy val bottomLeft: Point =
      Point(bottomRight.x, topLeft.y)

    // Is this point inside this box
    def contains(point: Point): Boolean = {
      point.x >= topLeft.x && point.x <= bottomRight.x &&
      point.y >= topLeft.y && point.y <= bottomRight.y
    }

    // Check if the two boxes overlap by comparing their corners
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
