package day4

import org.scalatest.funsuite.AnyFunSuite

class Day4Test extends AnyFunSuite {
  test("hasAdjacentDigits") {
    assert(!Day4.hasAdjacentDigits(12345))
    assert(Day4.hasAdjacentDigits(11345))
    assert(Day4.hasAdjacentDigits(12245))
  }

  test("hasNoDecreasingDigits") {
    assert(Day4.hasNoDecreasingDigits(12345))
    assert(Day4.hasNoDecreasingDigits(11345))
    assert(!Day4.hasNoDecreasingDigits(21345))
  }

  test("hasThreeOrMoreAdjacentDigits") {
//    assert(Day4.hasExactlyTwoAdjacentDigits(12245))
//    assert(!Day4.hasExactlyTwoAdjacentDigits(11145))
//    assert(!Day4.hasExactlyTwoAdjacentDigits(11115))
    assert(Day4.hasExactlyTwoAdjacentDigits(11144))
  }
}
