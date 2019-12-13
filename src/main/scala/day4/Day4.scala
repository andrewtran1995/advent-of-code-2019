package day4

//import scala.collection.parallel.CollectionConverters._

object Day4 {
  final val INPUT = "402328-864247"
  def main(args: Array[String]): Unit = {
    val (start, end) = INPUT.split("-") match { case Array(x: String, y: String) => (x.toInt, y.toInt) }
    val passwords = passwordsFromRange((start to end).toList)
    println("there are %s eligible passwords from the range" format passwords.length)
    val passwordsStrict = passwordsFromRangeStrict((start to end).toList)
    println("there are %s eligible passwords from the range meeting the new requirement" format passwordsStrict.length)
  }

  def passwordsFromRange(nums: List[Int]): List[Int] = nums.filter(isValidPassword)

  def passwordsFromRangeStrict(nums: List[Int]): List[Int] = nums.filter(isValidPasswordStrict)

  def isValidPassword(num: Int): Boolean = hasAdjacentDigits(num) && hasNoDecreasingDigits(num)

  def isValidPasswordStrict(num: Int): Boolean = hasExactlyTwoAdjacentDigits(num) && hasNoDecreasingDigits(num)

  def hasAdjacentDigits(num: Int): Boolean = {
    numToDigits(num).foldLeft((false, 0)){ case ((hasAdjacentDigits: Boolean, digit: Int), next: Int) =>
      if (hasAdjacentDigits) (true, next)
      else if (digit == next) (true, next)
      else (false, next)
    }
  }._1

  def hasExactlyTwoAdjacentDigits(num: Int): Boolean = {
    val (conditionMet, _, count) = numToDigits(num).foldLeft((false, 0, 0)){ case ((hasExactlyTwoAdjacentDigits: Boolean, digit: Int, count: Int), next: Int) =>
      if (hasExactlyTwoAdjacentDigits) (true, next, count)
      else if (digit != next) {
        if (count == 1) (true, next, 0)
        else (hasExactlyTwoAdjacentDigits, next, 0)
      } else (hasExactlyTwoAdjacentDigits, next, count+1)
    }
    if (conditionMet) true
    else if (count == 1) true
    else false
  }

  def hasNoDecreasingDigits(num: Int): Boolean = {
    numToDigits(num).foldLeft((true, 0)){ case ((hasNoDecreasingDigits: Boolean, digit: Int), next: Int) =>
      if (hasNoDecreasingDigits) {
        (digit <= next, next)
      } else {
        (false, next)
      }
    }._1
  }

  def numToDigits(num: Int): IndexedSeq[Int] = num.toString.map(_.asDigit)
}
