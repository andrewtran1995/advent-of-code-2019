package day2

import scala.io.Source
import scala.util.control.Breaks._

object Day2 {
  val filePath = "src/main/resources/day2/input"
  def main(args: Array[String]) : Unit = {
    // Read the file.
    val intcode = readFileIntoArray(filePath)
    breakable {
      for (i <- 0 until 100) {
        for (j <- 0 until 100) {
          val candidateIntcode = initIntcodeValues(intcode, i, j)
          val (_, processedIntcode) = stepIntcode(0, candidateIntcode)
          if (processedIntcode(0) == 19690720) {
            // Print value at position 1 and 2.
            println("Value at position 1 is %s, and position 2 is %s; final answer is %s".format(
              processedIntcode(1),
              processedIntcode(2),
              100 * processedIntcode(1) + processedIntcode(2)))
            break
          }
        }
      }
    }
  }

  def readFileIntoArray(filePath: String) : Array[Int] = {
    val bufferedSource = Source.fromFile(filePath)
    val text = (for (line <- bufferedSource.getLines()) yield line).toArray
    bufferedSource.close()
    text.apply(0).split(",").map(str => str.toInt)
  }

  def stepIntcode(opAddress: Int, intcode: Array[Int]) : (Int, Array[Int]) = {
    val returnIntcode: Array[Int] = new Array[Int](intcode.length)
    intcode copyToArray returnIntcode
    intcode(opAddress) match {
      case 1 | 2 =>
        val firstIdx = intcode(opAddress+1)
        val secondIdx = intcode(opAddress+2)
        val position = intcode(opAddress+3)
        intcode(opAddress) match {
          case 1 =>
            returnIntcode(position) = intcode(firstIdx) + intcode(secondIdx)
            stepIntcode(opAddress + 4, returnIntcode)
          case 2 =>
            returnIntcode(position) = intcode(firstIdx) * intcode(secondIdx)
            stepIntcode(opAddress + 4, returnIntcode)
        }
      case 99 =>
        (0, returnIntcode)
    }
  }

  def initIntcodeValues(intcodeArg: Array[Int], val1: Int, val2: Int) : Array[Int] = {
    val intcode : Array[Int] = new Array[Int](intcodeArg.length)
    intcodeArg copyToArray intcode
    intcode(1) = val1
    intcode(2) = val2
    intcode
  }
}
