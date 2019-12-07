package day2

import org.scalatest.funsuite.AnyFunSuite

class Day2Test extends AnyFunSuite {
  test("simple intcode program") {
    assert(Day2.stepIntcode(0, Array(1, 0, 0, 0, 99))._2 sameElements Array(2, 0, 0, 0, 99))
    val (_, result) = Day2.stepIntcode(0, Array(1,1,1,4,99,5,6,0,99))
    assert(result sameElements Array(30,1,1,4,2,5,6,0,99))
  }
}
