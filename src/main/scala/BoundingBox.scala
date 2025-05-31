package org.amizan

import BoundingBoxHelperFunctions._

import scala.io.Source

object BoundingBox {
  def main(args: Array[String]): Unit = {
    val lines = Source.stdin.getLines().toList
    val points = parseInput(lines)
    val boxes = findBoundingBoxes(points)
    boxes.foreach(println)
    val nonOverlappingBoxes = findNonOverlappingBoxes(boxes)
    nonOverlappingBoxes.foreach(println)
  }
}
