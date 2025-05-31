package org.amizan

import scala.io.Source

object BoundingBox extends App {
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

  def toBool(c: Char, row: => Int, col: => Int): Boolean = c match {
    case '-' => false
    case '*' => true
    case _ =>
      throw new IllegalArgumentException(
        s"Bad Input Character at: Row $row Col $col: $c"
      )
  }

  val linesIterator = Source.stdin.getLines()
  val lines = linesIterator.toList
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
      Some(Point(i,j))
    else
      None
  }

  maybePoints.flatten.toList.foreach(println)

  val points: List[Point] = maybePoints.flatten.toList

  def findAdjacentPointsInUnvisited(
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

  def buildBox(points: List[Point]): Box = {
    val xs = points.map(_.x)
    val ys = points.map(_.y)

    val topLeft = Point(xs.min, ys.min)
    val bottomRight = Point(xs.max, ys.max)

    Box(topLeft, bottomRight)
  }

  @annotation.tailrec
  def dfs(
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

  val boxes = dfs(
    visitQueue = points.headOption.toList, // will not be None
    unvisitedPoints = points.tail.toSet,
    pointsAcc = Nil,
    boxAcc = Nil,
  )

  val oneIndexedBoxes = boxes.map(_.plusOne)

  oneIndexedBoxes.foreach(println)
}
