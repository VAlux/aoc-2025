import d2p1.{Range, *}

import scala.annotation.tailrec

object d2p2 extends Solution[Long]:

  def expandRange(range: Range): List[Id] =
    (range.from to range.to)
      .map(_.toString)
      .toList

  def checkId(id: Id): Boolean =
    def check(indexes: List[Int]): Boolean =
      indexes.map(index => id.substring(index, index + id.length / indexes.size)).toSet.size == 1

    if id.length == 1 then false
    else if id.toSet.size == 1 then true
    else if id.length > 3 then
      val indexes = (2 to id.length / 2)
        .filter(step => id.length % step == 0)
        .map(step => (0 until id.length by step).toList)
        .toList

      indexes.exists(check)
    else false

  override def solve(input: List[String]): Long =
    val ranges     = parseRanges(input.head)
    val ids        = ranges.flatMap(expandRange)
    val invalidIds = ids.filter(id => checkId(id))
    invalidIds.map(_.toLong).sum
