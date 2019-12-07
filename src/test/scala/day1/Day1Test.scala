package day1

import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite {
  test("fuelRequired") {
    val cases : List[(Int, Int)] = List((0, 0), (-1, 0), (11, 1), (12, 2), (13, 2))
    cases.foreach { case (arg, want) => assert(Day1.fuelRequired(arg) == want) }
  }

  test("fuelRequiredRecursive") {
    val cases : List[(Int, Int)] = List((0, 0), (-1, 0), (11, 1), (12, 2))
    cases.foreach { case (arg, want) => assert(Day1.fuelRequiredRecursive(arg) == want)}
  }
}
