package day3
import scala.io.Source

object Day3 {
  final val filePath = "src/main/resources/day3/input"
  def main(args: Array[String]): Unit = {
    val bufferedSource = Source.fromFile(filePath)
    val (path1, path2) = bufferedSource.getLines().map(_.split(',')).toList match {
      case List(x: Array[String], y: Array[String]) => (x.toList, y.toList)
    }
    bufferedSource.close
    val (wirepaths1, wirepaths2) = (path1.map(WirePath(_)), path2.map(WirePath(_)))
    println("shortest distance is %s" format findShortestDistance(wirepaths1, wirepaths2))
    println("fewest steps is %s" format findFewestSteps(wirepaths1, wirepaths2))
  }

  def findShortestDistance(paths1: List[WirePath], paths2: List[WirePath]): Int = {
    val possiblePairs = for (
      line1 <- WirePath.pathsToLines(paths1);
      line2 <- WirePath.pathsToLines(paths2)
    ) yield (line1, line2)
    possiblePairs.flatMap{case (l1, l2) => l1.intersects(l2)}.filter(_ != Point.start).map(Line(Point.start, _).manhattanDistance).min
  }

  def findFewestSteps(paths1: List[WirePath], paths2: List[WirePath]): Int = {
    val lines1 = WirePath.pathsToLines(paths1)
    val lines2 = WirePath.pathsToLines(paths2)
    val possiblePairs = for (
      (line1, i) <- lines1.reverse.zipWithIndex;
      (line2, j) <- lines2.reverse.zipWithIndex
    ) yield (line1, i, line2, j)
    val (line1: Line, _, line2: Line, _) = possiblePairs.filter{ case (l1, _, l2, _) => l1.intersects(l2).exists(_ != Point.start) }.minBy{ case (_, i, _, j) => i + j }
    line1.intersects(line2).map(pt => stepsUntilPoint(lines1, pt) + stepsUntilPoint(lines2, pt)).min
  }

  def stepsUntilPoint(lines: List[Line], point: Point): Int = {
    lines.foldLeft((0, false)){ case ((steps: Int, done: Boolean), line: Line) =>
      if (done) (steps, done)
      else if (line.intersects(Line(point, point)).nonEmpty) {
        (steps + line.stepsToPoint(point), true)
      } else {
        (steps + line.manhattanDistance, false)
      }
    }._1
  }

  case class Point(x: Int, y: Int) {
    def withWirePath(w: WirePath): Point = w.direction match {
      case Direction.U => Point(x + w.steps, y)
      case Direction.D => Point(x - w.steps, y)
      case Direction.L => Point(x, y - w.steps)
      case Direction.R => Point(x, y + w.steps)
    }
  }
  object Point {
    def start: Point = Point(1,1)
  }
  case class Line(p1: Point, p2: Point) {
    def intersects(other: Line): List[Point] = {
      isCollinearBy(other) map {
        case 'X' =>
          getRangeOverlap((p1.x, p2.x), (other.p1.x, other.p2.x)) map {case (s, e) => s to e map (Point(_, p1.y))} getOrElse List() toList
        case 'Y' =>
          getRangeOverlap((p1.y, p2.y), (other.p1.y, other.p2.y)) map {case (s, e) => s to e map (Point(p1.x, _))} getOrElse List() toList
      } getOrElse {
        if (isParallel(other)) {
          List()
        } else if (isHorizontal && (xRange contains other.p1.x) && (other.yRange contains p1.y)) {
          List(Point(other.p1.x, p1.y))
        } else if (isVertical && (yRange contains other.p1.y) && (other.xRange contains p1.x)) {
          List(Point(p1.x, other.p1.y))
        } else {
          List()
        }
      }
    }

    def isVertical: Boolean = p1.x == p2.x
    def isHorizontal: Boolean = p1.y == p2.y

    def xRange: Range = if (p1.x <= p2.x) p1.x to p2.x else p2.x to p1.x
    def yRange: Range = if (p1.y <= p2.y) p1.y to p2.y else p2.y to p1.y

    def isParallel(other: Line): Boolean = isVertical && other.isVertical || isHorizontal && other.isHorizontal

    def isCollinearBy(other: Line): Option[Char] = {
      if (this.isParallel(other)) {
        if (isVertical && p1.x == other.p1.x) Some('Y')
        else if (isHorizontal && p1.y == other.p1.y) Some('X')
        else None
      } else None
    }

    def getRangeOverlap(range1: (Int, Int), range2: (Int, Int)): Option[(Int, Int)] = {
      val thisRange = if (range1._1 > range1._2) range1.swap else range1
      val otherRange = if (range2._1 > range2._2) range2.swap else range2
      val overlap = (math.max(thisRange._1, otherRange._1), math.min(thisRange._2, otherRange._2))
      if (overlap._1 <= overlap._2)
        Some(overlap)
      else
        None
    }

    def manhattanDistance: Int = math.abs(p1.x - p2.x) + math.abs(p1.y - p2.y)

    def stepsToPoint(point: Point): Int = math.abs(point.x - p1.x) + math.abs(point.y - p1.y)
  }
  case class WirePath(direction: Direction.Value, steps: Int)
  object WirePath {
    def apply(s: String): WirePath = WirePath(Direction.withName(s.head.toString), s.tail.toInt)

    // Fold a list of wirepaths to a list of lines (starting at Point.start).
    // Note that the returned list is in "reverse" order, but order for lines should not matter functionally.
    def pathsToLines(wirepaths: List[WirePath]): List[Line] = {
      wirepaths.foldLeft(List(): List[Line]) { (lines: List[Line], w: WirePath) =>
        if (lines.isEmpty) List(Line(Point.start, Point.start.withWirePath(w)))
        else Line(lines.head.p2, lines.head.p2.withWirePath(w)) :: lines
      }
    }
  }
  class Direction
  object Direction extends Enumeration {
    type Direction = Value
    val U, D, L, R = Value
  }
}
