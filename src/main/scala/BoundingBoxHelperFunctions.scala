package org.amizan

import BoundingBoxDataClasses.{Box, Point}

object BoundingBoxHelperFunctions {
  def parseInput(lines: List[String]): List[Point] = {
    val matrix: Array[Array[Boolean]] =
      lines
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

  def findBoundingBoxes(points: List[Point]): List[Box] =
    dfs(
      visitQueue = points.headOption.toList,
      unvisitedPoints = points.tail.toSet,
      pointsAcc = Nil,
      boxAcc = Nil,
    )

  def findNonOverlappingBoxes(boxes: List[Box]): List[Box] = {
    @annotation.tailrec
    def helper(
      remainingBoxes: List[Box],
      retAcc: List[Box],
    ): List[Box] = remainingBoxes match {
      case Nil => retAcc
      case box :: rest =>
        val boxesThatOverlapWithThisOne = rest.filter(_.overlaps(box))
        if(boxesThatOverlapWithThisOne.isEmpty) {
          helper(rest, box :: retAcc)
        }
        else {
          val remainingBoxesToProcess = rest.toSet &~ boxesThatOverlapWithThisOne.toSet
          helper(remainingBoxesToProcess.toList, retAcc)
        }
    }
    helper(boxes, Nil)
  }

  private def toBool(c: Char, row: => Int, col: => Int): Boolean = c match {
    case '-' => false
    case '*' => true
    case _ =>
      throw new IllegalArgumentException(
        s"Bad Input Character at: Row $row Col $col: $c"
      )
  }

  private def findAdjacentPointsInUnvisited(
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

  private def buildBox(points: List[Point]): Box = {
    val xs = points.map(_.x)
    val ys = points.map(_.y)

    val topLeft = Point(xs.min, ys.min)
    val bottomRight = Point(xs.max, ys.max)

    Box(topLeft, bottomRight)
  }

  @annotation.tailrec
  private def dfs(
    visitQueue: List[Point],
    unvisitedPoints: Set[Point],
    pointsAcc: List[Point],
    boxAcc: List[Box],
  ): List[Box] = visitQueue match {
    case Nil =>
      val boxes = buildBox(pointsAcc) :: boxAcc
      unvisitedPoints.headOption match {
        case None => boxes
        case Some(unvisitedPoint) =>
          dfs(
            visitQueue = List(unvisitedPoint),
            unvisitedPoints = unvisitedPoints - unvisitedPoint,
            pointsAcc = Nil,
            boxAcc = boxes,
          )
      }
    case point :: rest =>
      val (pointsToVisit, remainingUnvisited) = findAdjacentPointsInUnvisited(
        point,
        unvisitedPoints,
      )
      dfs(
        visitQueue = rest ::: pointsToVisit.toList,
        unvisitedPoints = remainingUnvisited,
        pointsAcc = point :: pointsAcc,
        boxAcc = boxAcc,
      )
  }
}
