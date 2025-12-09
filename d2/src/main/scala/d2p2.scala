import d2p1.{Range, *}

import scala.annotation.tailrec

object d2p2 extends Solution[Long]:

  def expandRange(range: Range): List[Id] =
    (range.from to range.to).map(_.toString).toList

  def checkId(id: Id): Boolean =
    def checkBreakpoints(breakPoints: List[Int]): Boolean =
      breakPoints.map(index => id.substring(index, index + id.length / breakPoints.size)).toSet.size == 1

    if id.length == 1 then false         // single digit is valid
    else if id.toSet.size == 1 then true // there are more then 1 digit and all of them are the same
    else if id.length <= 3 then false // no need to check this case, can't be invalid by definition
    else
      val breakPoints = (2 to id.length / 2)
        .filter(step => id.length % step == 0)
        .map(step => (0 until id.length by step).toList)
        .toList

      breakPoints.exists(checkBreakpoints)

  override def solve(input: List[String]): Long =
    parseRanges(input.head).flatMap(expandRange).filter(id => checkId(id)).map(_.toLong).sum
