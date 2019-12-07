package day1

import scala.io.Source

object Day1 {
  def main(args: Array[String]): Unit = {
    val filePath = "src/main/resources/day1/input"
    val bufferedSource = Source.fromFile(filePath)
//    val totalFuelRequired = bufferedSource.getLines().map(str => fuelRequired(str.toInt)).sum
//    println(s"The total amount of fuel required is $totalFuelRequired")
    val totalFuelRequiredRecursive = bufferedSource.getLines().map(str => fuelRequiredRecursive(str.toInt)).sum
    println(s"The total amount of fuel required (recursively) is $totalFuelRequiredRecursive")
    bufferedSource.close()
  }
  def fuelRequired(weight: Int): Int = {
    math.max(weight / 3 - 2, 0)
  }

  def fuelRequiredRecursive(weight: Int): Int = {
    if (weight <= 0) {
      return 0
    }
    val fuel = math.max(weight / 3 - 2, 0)
    fuel + fuelRequiredRecursive(fuel)
  }
}