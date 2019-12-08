package day3

object Day3 {
  case class Point(x: Int, y: Int)
  case class Line(p1: Point, p2: Point) {
    def intersects(other: Line): List[Point] = {
      this.isCollinearBy(other) match {
        case 'X' =>
          getRangeOverlap((p1.x, p2.x), (other.p1.x, other.p2.x)) match {
            case Some((start, end)) => (start to end).map(Point(_, p1.y)).toList
            case None => List()
          }
        case 'Y' =>
          getRangeOverlap((p1.y, p2.y), (other.p1.y, other.p2.y)) match {
            case Some((start, end)) => (start to end).map(Point(p1.x, _)).toList
            case None => List()
          }
        case None =>
          if (this.isParallel(other)) {
            List()
          } else if (isVertical && (p1.x to p2.x contains other.p1.x) && (other.p1.y to other.p2.y contains p1.y)) {
            List(Point(p1.x, other.p1.y))
          } else if ((p1.y to p2.y contains other.p1.y) && (other.p1.x to other.p2.x contains p1.x)) {
            List(Point(other.p1.x, p1.y))
          } else {
            List()
          }
      }
    }

    def isVertical: Boolean = p1.x == p2.x
    def isHorizontal: Boolean = p1.y == p2.y
    def isParallel(other: Line): Boolean = isVertical && other.isVertical || isHorizontal && other.isHorizontal

    def isCollinearBy(other: Line): Option[Character] = {
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
  }

  def main(args: Array[String]): Unit = {

  }

}
