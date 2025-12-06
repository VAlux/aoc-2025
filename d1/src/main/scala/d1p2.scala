import d1p1.*

import scala.annotation.tailrec

object d1p2 extends Solution[Int]:
  override def solve(input: List[String]): Int =
    val rotations = parseRotations(input)
    val results   = rotateAll(rotations = rotations)
    results.map(_.zeros).sum

  case class DialState(current: Int, zerosDuringRotation: Int = 0):
    def zeros: Int = if current == 0 then zerosDuringRotation + 1 else zerosDuringRotation

  @tailrec
  def rotateAll(current: Int = 50, rotations: List[Rotation], acc: List[DialState] = List.empty): List[DialState] =
    if rotations.isEmpty then acc
    else
      val newValue = rotate(current = current, rotation = rotations.head)
      rotateAll(newValue.current, rotations.tail, acc :+ newValue)

  private def calculateDialState(fullRotations: Int, crossesZeroDuringRemainder: Boolean, newValue: Int): DialState =
    val totalZeros          = fullRotations + (if crossesZeroDuringRemainder then 1 else 0)
    val zerosDuringRotation = totalZeros - (if newValue == 0 then 1 else 0)

    DialState(newValue, zerosDuringRotation)

  def rotate(current: Int, rotation: Rotation): DialState =
    val fullRotations = rotation.points / dialDimension
    val remainder     = rotation.points % dialDimension

    rotation.direction match
      case Direction.Left  =>
        val clicks                     = current - remainder
        val newValue                   = if clicks < 0 then dialDimension + clicks else clicks
        val crossesZeroDuringRemainder = current > 0 && remainder >= current
        calculateDialState(fullRotations, crossesZeroDuringRemainder, newValue)
      case Direction.Right =>
        val newValue                   = (current + rotation.points) % dialDimension
        val crossesZeroDuringRemainder = current > 0 && remainder >= (dialDimension - current)
        calculateDialState(fullRotations, crossesZeroDuringRemainder, newValue)
