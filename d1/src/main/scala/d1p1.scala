import scala.annotation.tailrec

object d1p1 extends Solution[Int]:
  private val dialMax: Int = 99
  val dialDimension: Int   = dialMax + 1

  override def solve(input: List[String]): Int =
    val rotations = parseRotations(input)
    val results   = rotateAll(rotations = rotations)
    results.count(_ == 0)

  enum Direction:
    case Left
    case Right

  object Direction:
    def fromString(value: String): Option[Direction] =
      value.toUpperCase() match
        case "L" => Some(Left)
        case "R" => Some(Right)
        case _   => None

  case class Rotation(direction: Direction, points: Int)

  def parseRotations(input: List[String]): List[Rotation] =
    input.flatMap: entry =>
      entry.splitAt(1) match
        case (direction, points) => Direction.fromString(direction).map(Rotation(_, points.toInt))
        case null                => None

  @tailrec
  def rotateAll(current: Int = 50, rotations: List[Rotation], acc: List[Int] = List.empty): List[Int] =
    if rotations.isEmpty then acc
    else
      val newValue = rotate(current = current, rotation = rotations.head)
      rotateAll(newValue, rotations.tail, acc :+ newValue)

  def rotate(current: Int, rotation: Rotation): Int =
    rotation.direction match
      case Direction.Left  =>
        val value = current - (rotation.points % dialDimension)
        if value < 0 then dialDimension + value else value
      case Direction.Right => (current + rotation.points) % dialDimension
