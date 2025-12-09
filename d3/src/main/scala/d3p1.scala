import scala.annotation.tailrec
import scala.util.chaining.*

object d3p1 extends Solution[Int]:

  type Joltage = Int

  case class BatteryBank(batteryJoltage: List[Joltage])

  def parseBatteryBanks(input: List[String]): List[BatteryBank] =
    input.map(line => BatteryBank(line.toCharArray.map(_.toString.toInt).toList))

  def computeSuffixes(battery: BatteryBank): List[Int] =
    battery.batteryJoltage.init
      .foldRight(List(battery.batteryJoltage.last)) { (joltage, suffixes) =>
        Math.max(suffixes.head, joltage) :: suffixes
      }
      .drop(1)

  def findMaxJoltage(battery: BatteryBank, suffixes: List[Int]): Int =
    suffixes.zipWithIndex.map((suffix, index) => (10 * battery.batteryJoltage(index)) + suffix).max

  override def solve(input: List[String]): Int =
    val banks = parseBatteryBanks(input)
    banks.zip(banks.map(computeSuffixes)).map((bank, suffix) => findMaxJoltage(bank, suffix)).sum
