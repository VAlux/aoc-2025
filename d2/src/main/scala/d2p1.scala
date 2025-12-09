import scala.annotation.tailrec

object d2p1 extends Solution[Long]:

  case class Range(from: Long, to: Long)

  type Id = String

  def parseRanges(input: String): List[Range] =
    def parseRange(rangeContent: String): Option[Range] =
      rangeContent.split("-") match
        case Array(from, to) => Some(Range(from.toLong, to.toLong))
        case _               => None

    input.split(",").flatMap(parseRange).toList

  def expandRange(range: Range): List[Id] =
    (range.from to range.to).map(_.toString).filter(number => number.length % 2 == 0).toList

  def checkId(id: Id): Boolean =
    @tailrec
    def check(leftIndex: Int, rightIndex: Int): Boolean =
      if rightIndex == id.length then true
      else if id(leftIndex) != id(rightIndex) then false
      else check(leftIndex + 1, rightIndex + 1)

    check(0, id.length / 2)

  override def solve(input: List[String]): Long =
    parseRanges(input.head).flatMap(expandRange).filter(id => checkId(id)).map(_.toLong).sum