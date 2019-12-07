package day2

import scala.io.Source

object Day2 {
  val filePath = "src/main/resources/day2/input"
  def main(args: Array[String]) : Unit = {
    readFileIntoArray(filePath)
  }

  def readFileIntoArray(filePath: String) : Array[Int] = {
    val bufferedSource = Source.fromFile(filePath)
    val text = (for (line <- bufferedSource.getLines()) yield line).toArray
    bufferedSource.close()
    text.apply(0).split(",").map(str => str.toInt)
  }
}
