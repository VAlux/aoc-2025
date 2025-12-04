import d1p1.*
import scala.annotation.tailrec

object d1p2 extends Solution[Int]:
  override def solve(input: List[String]): Int =
    val rotations = parseRotations(input)
    val results   = rotateAll(rotations = rotations)
    println(results.mkString("\n"))
    results.map(_.zeros).sum

  case class DialState(current: Int, zerosDuringRotation: Int = 0):
    def zeros: Int = if current == 0 then zerosDuringRotation + 1 else zerosDuringRotation

  @tailrec
  def rotateAll(current: Int = 50, rotations: List[Rotation], acc: List[DialState] = List.empty): List[DialState] =
    if rotations.isEmpty then acc
    else
      val newValue = rotate(current = current, rotation = rotations.head)
      rotateAll(newValue.current, rotations.tail, acc :+ newValue)

  def rotate(current: Int, rotation: Rotation): DialState =
    val fullRotations: Int = rotation.points / 100

    rotation.direction match
      case Direction.Left  =>
        val clicks   = current - (rotation.points % 100)
        val newValue = if clicks < 0 then 100 + clicks else clicks
        val zeros    = if rotation.points > current && current != 0 then fullRotations + 1 else fullRotations

        DialState(newValue, zeros)
      case Direction.Right =>
        val newValue = (current + rotation.points) % 100
        val zeros    = if rotation.points > (100 - current) && current != 0 then fullRotations + 1 else fullRotations
        DialState(newValue, zeros)
