package day3

import day3.Day3.{Line, Point}
import org.scalatest.funsuite.AnyFunSuite

class Day3Test extends AnyFunSuite {
  test("Point instantiation") {
    assert(Point(1,2) == Point(1,2))
    assert(Point(1,2) != Point(1,3))
  }

  test("Line instantiation") {
    val (p1, p2, p3) = (Point(1,1), Point(1,2), Point(1,3))
    assert(Line(p1, p2) == Line(p1, p2))
    assert(Line(p1, p2) != Line(p1, p3))
  }

  test("isHorizontal and isVertical") {
    val (l1, l2, l3) = (Line(Point(1,1), Point(1,3)), Line(Point(1,1), Point(1,2)), Line(Point(1,1), Point(3,1)))
    assert(!l1.isHorizontal)
    assert(l1.isVertical)
    assert(!l2.isHorizontal)
    assert(l2.isVertical)
    assert(l3.isHorizontal)
    assert(!l3.isVertical)
  }

  test("xRange and yRange") {
    val l1 = Line(Point(0,0), Point(10,0))
    assert(l1.xRange == (0 to 10))
    assert(l1.yRange == (0 to 0))

    val l2 = Line(Point(0,0), Point(10,10))
    assert(l2.xRange == (0 to 10))
    assert(l2.yRange == (0 to 10))

    val l3 = Line(Point(0,0), Point(0,10))
    assert(l3.xRange == (0 to 0))
    assert(l3.yRange == (0 to 10))

    val l4 = Line(Point(10,10), Point(0,0))
    assert(l4.xRange == (0 to 10))
    assert(l4.yRange == (0 to 10))
  }

  test("isParallel and isCollinearBy") {
    val v1 = Line(Point(0,0), Point(0,10))
    val v2 = Line(Point(1,0), Point(1,10))
    val h1 = Line(Point(0,0), Point(10,0))
    val h2 = Line(Point(0,1), Point(10,1))
    assert(v1.isParallel(v2))
    assert(!v1.isParallel(h1))
    assert(!v1.isParallel(h2))

    val v3 = Line(Point(0,9), Point(0,10))
    val v4 = Line(Point(0,10), Point(0,11))
    assert(v1.isParallel(v3))
    assert(v1.isParallel(v4))
    assert(v1.isCollinearBy(v3).map(_ == 'Y').head)
    assert(v1.isCollinearBy(v4).map(_ == 'Y').head)
    assert(v1.isCollinearBy(v2).isEmpty)

    val h3 = Line(Point(9,0), Point(10,0))
    val h4 = Line(Point(10,0), Point(11,0))
    val h5 = Line(Point(11,0), Point(12,0))
    assert(h1.isParallel(h3))
    assert(h1.isParallel(h4))
    assert(h1.isParallel(h5))
    assert(h1.isCollinearBy(h3).map(_ == 'X').head)
    assert(h1.isCollinearBy(h4).map(_ == 'X').head)
    assert(h1.isCollinearBy(h5).map(_ == 'X').head)
    assert(h1.isCollinearBy(h2).isEmpty)
  }

  test("getRangeOverlap") {
    val l1 = Line(Point(0,0), Point(0,10))
    val l2 = Line(Point(0,8), Point(0,10))
    val l3 = Line(Point(0,8), Point(0,12))
    val l4 = Line(Point(0,10), Point(0,12))
    val l5 = Line(Point(0,12), Point(0,14))

    assert(l1.getRangeOverlap((l1.p1.y, l1.p2.y), (l2.p1.y, l2.p2.y)).get == (8, 10))
    assert(l1.getRangeOverlap((l1.p1.y, l1.p2.y), (l3.p1.y, l3.p2.y)).get == (8, 10))
    assert(l1.getRangeOverlap((l1.p1.y, l1.p2.y), (l4.p1.y, l4.p2.y)).get == (10, 10))
    assert(l1.getRangeOverlap((l1.p1.y, l1.p2.y), (l5.p1.y, l5.p2.y)).isEmpty)
  }

  test("intersects") {
    val l1 = Line(Point(3,0), Point(3, 10))
    val l2 = Line(Point(3,8), Point(3, 12))
    val l3 = Line(Point(2,1), Point(4,1))
    val l4 = Line(Point(2,14), Point(4,14))

    assert(l1.intersects(l2) == (8 to 10).map(Point(3,_)).toList)
    assert(l1.intersects(l3) == List(Point(3,1)))
    assert(l1.intersects(l4) == List())
  }
}
