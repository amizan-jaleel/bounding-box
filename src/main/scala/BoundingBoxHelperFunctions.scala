package org.amizan

import BoundingBoxDataClasses.{Box, Point}

object BoundingBoxHelperFunctions {
  /**
   * Parse the input file into a list of `Point`.
   * Each point corresponds to an asterisk ('*') in the input. We validate the input,
   * ensuring all rows have consistent lengths and throws an exception if the input is invalid.
   *
   * @param lines A list of strings representing a grid where '-' denotes empty space
   *              and '*' denotes a point.
   * @return A list of Point instances corresponding to the positions
   *         of asterisks ('*') in the input grid.
   * @throws IllegalArgumentException if the input is empty or if the rows have inconsistent lengths.
   */
  def parseInput(lines: List[String]): List[Point] = {
    val matrix: Array[Array[Boolean]] =
      lines
        .view
        .zipWithIndex
        .map { case (line, rowNum) =>
          line.toCharArray.zipWithIndex.map { case (ch, colNum) =>
            toBool(ch, row = rowNum, col = colNum)
          }
        }
        .toArray

    val firstRow = matrix.headOption.getOrElse {
      throw new IllegalArgumentException("Empty input!")
    }
    val numCols = firstRow.length
    matrix.zipWithIndex.tail.foreach { case (row, index) =>
      if(row.length != numCols) {
        throw new IllegalArgumentException(s"Row $index is not the same size as the other rows!")
      }
    }

    val numRows = matrix.length
    val maybePoints = for {
      i <- 0 until numRows
      j <- 0 until numCols
    } yield {
      if(matrix(i)(j))
        Some(Point(i+1,j+1))
      else
        None
    }

    maybePoints.flatten.toList
  }

  /**
   * Find all the bounding boxes from these points.
   * Calls the tail recursive helper function
   *
   * @param points A list of points that corresponds to the asterisks in the input file
   * @return A list of Box instances. Each Box represents the smallest rectangle that can contain
   *         a contiguous group of points from the input list.
   */
  def findBoundingBoxes(points: List[Point]): List[Box] = points match {
    case Nil => Nil
    case head :: tail =>
      pointsToBoxes(
        visitQueue = List(head),
        unvisitedPoints = tail.toSet,
        pointsAcc = Nil,
        boxAcc = Nil,
      )
  }

  /**
   * Given a list of boxes, throw out any box that overlaps with any other box
   * Use the `overlap` method on the `Box` class to test for overlap
   *
   * Runtime Complexity:
   * - In the worst case, for each box in the list, the function may compare it with all other boxes.
   * - If there are `n` boxes, the total comparisons can approach O(n2) in such cases.
   * - This can happen if you have lots of tiny boxes that all don't overlap with each other
   *
   * @param boxes A list of Box instances to evaluate for overlapping.
   * @return A filtered down list of boxes that all don't overlap with one another
   */
  def findNonOverlappingBoxes(boxes: List[Box]): List[Box] = {
    @annotation.tailrec
    def helper(
      remainingBoxes: List[Box],
      retAcc: List[Box],
    ): List[Box] = remainingBoxes match {
      case Nil => retAcc // Done comparing boxes, return the accumulator
      case box :: rest => // Find all the boxes that overlap with this one
        val boxesThatOverlapWithThisOne = rest.filter(_.overlaps(box))
        if(boxesThatOverlapWithThisOne.isEmpty) {
          // We can keep this box, it doesn't overlap with anything
          helper(rest, box :: retAcc)
        }
        else {
          // Throw out this box and all the ones that overlap with it
          val remainingBoxesToProcess = rest.toSet &~ boxesThatOverlapWithThisOne.toSet
          helper(remainingBoxesToProcess.toList, retAcc)
        }
    }
    helper(boxes, Nil)
  }

  /**
   * Converts a given character into a boolean value based on specific character rules.
   * Throws an IllegalArgumentException if the character is not valid.
   *
   * @param c   The input character to be converted. 
   *            Acceptable characters: '-' (false), '*' (true).
   * @param row The row number where the character is located; used for error reporting.
   *            This parameter is lazily evaluated.
   * @param col The column number where the character is located; used for error reporting.
   *            This parameter is lazily evaluated.
   * @return A boolean value corresponding to the input character: 
   *         false for '-', true for '*'.
   * @throws IllegalArgumentException if the character is neither '-' nor '*'.
   */
  private[amizan] def toBool(c: Char, row: => Int, col: => Int): Boolean = c match {
    case '-' => false
    case '*' => true
    case _ =>
      throw new IllegalArgumentException(
        s"Bad Input Character at: Row $row Col $col: $c"
      )
  }

  /**
   * For a given point, check up, down, left, and right
   * If any of those are in the 'to visit' group, return those
   * Also return the 'to visit' group without the above accounted for points
   *
   * @param point           Check if any of this point's neighbors should go in the box
   * @param unvisitedPoints Points not yet assigned to a Box
   * @return A tuple containing two sets:
   *         - The first set includes points adjacent to the given point that are also unvisited.
   *         - The second set includes the remaining unvisited points excluding the adjacent ones.
   */
  private[amizan] def findAdjacentPointsInUnvisited(
    point: Point,
    unvisitedPoints: Set[Point],
  ): (Set[Point], Set[Point]) = {
    val (x: Int, y: Int) = (point.x, point.y)
    val points = Set(
      Point(x-1, y),
      Point(x+1, y),
      Point(x, y-1),
      Point(x, y+1),
    )

    val pointsToVisit = unvisitedPoints & points
    val remainingUnvisited = unvisitedPoints &~ points

    (pointsToVisit, remainingUnvisited)
  }

  /**
   * Builds a bounding box that encompasses all the points in the provided list.
   *
   * @param points A list of points for which the bounding box is to be created. 
   *               The list must not be empty.
   * @return A Box instance representing the smallest rectangle that contains all the input points.
   */
  private[amizan] def buildBox(points: List[Point]): Box = {
    val xs = points.map(_.x)
    val ys = points.map(_.y)

    // At this point, an empty list should never be passed in
    // So it should be safe to do min/max instead of minOption/maxOption
    val topLeft = Point(xs.min, ys.min)
    val bottomRight = Point(xs.max, ys.max)

    Box(topLeft, bottomRight)
  }

  /***
   * Group all contiguous points and transform them to boxes
   *
   * @param visitQueue contiguous points, to be put into a box
   * @param unvisitedPoints all the points not yet in a box
   * @param pointsAcc when done searching this island, put this into a box
   * @param boxAcc accumulator for return
   * @return all bounding boxes
   */
  @annotation.tailrec
  private def pointsToBoxes(
    visitQueue: List[Point],
    unvisitedPoints: Set[Point],
    pointsAcc: List[Point],
    boxAcc: List[Box],
  ): List[Box] = visitQueue match {
    case Nil => // We've exhausted a DFS of adjacent points for this box
      val boxes = buildBox(pointsAcc) :: boxAcc // Save the box to the accumulator
      unvisitedPoints.headOption match {
        case None => boxes // We've found all boxes, return the result
        case Some(unvisitedPoint) => // There's another box to find
          pointsToBoxes(
            visitQueue = List(unvisitedPoint),
            unvisitedPoints = unvisitedPoints - unvisitedPoint,
            pointsAcc = Nil, // Fresh set of points for next box
            boxAcc = boxes,
          )
      }
    case point :: rest => // We're still searching for current box
      // Check adjacent points and add to queue
      val (pointsToVisit, remainingUnvisited) = findAdjacentPointsInUnvisited(
        point,
        unvisitedPoints,
      )
      pointsToBoxes(
        visitQueue = rest ::: pointsToVisit.toList,
        unvisitedPoints = remainingUnvisited,
        pointsAcc = point :: pointsAcc,
        boxAcc = boxAcc,
      )
  }
}
