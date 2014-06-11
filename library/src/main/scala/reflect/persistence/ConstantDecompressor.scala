package scala.reflect.persistence

class ConstantDecompressor[U <: scala.reflect.api.Universe](val u: U) {
  import Enrichments._

  def apply(toRead: List[Byte]): Map[Any, List[Int]] = {
    val (constSize, xs) = readInt(toRead)
    val constBytes: List[Byte] = xs.take(constSize)
    var rest: List[Byte] = xs.drop(constSize)
    val (occsSize, ys) = readInt(rest)
    rest = ys
    val occs: List[Int] = (0 until occsSize).map { x =>
      val (entry, zs) = readShort(rest)
      rest = zs
      entry.toInt
    }.toList

    def parseConstants(l: List[Byte]): List[Any] = l match {
      case Nil => Nil
      case t :: x :: xs if t == 'B' =>
        (x == 1.toByte) :: parseConstants(xs)
      case t :: x :: xs if t == 'b' =>
        x :: parseConstants(xs)
      case t :: xs if t == 's' =>
        val (x, xss) = readShort(xs)
        x :: parseConstants(xss)
      case t :: x :: xs if t == 'c' => x :: parseConstants(xs)
      case t :: xs if t == 'i' =>
        val (x, xss) = readInt(xs)
        x :: parseConstants(xss)
      case t :: xs if t == 'l' =>
        val (x, xss) = readLong(xs)
        x :: parseConstants(xss)
      case t :: xs if t == 'f' =>
        val (x, xss) = readFloat(xs)
        x :: parseConstants(xss)
      case t :: xs if t == 'd' =>
        val (x, xss) = readDouble(xs)
        x :: parseConstants(xss)
      case t :: xs if List('g', 'T', 'S').contains(t) =>
        val (size, rest1) = readShort(xs)
        rest1.take(size).map(c => c.toChar).mkString :: parseConstants(rest1.drop(size))
      case t :: xs if t == 'e' => () :: parseConstants(xs)
    }
    val constsList = parseConstants(constBytes).sortBy(v => v.toString)
    val grouped: List[List[Int]] = occs.zipWithIndex.groupBy(_._1).toList.sortBy(_._1).map { x =>
      x._2.map(y => y._2)
    }.toList
    assert(grouped.size == constsList.size, s"grouped.size: ${grouped.size}, constsList.size: ${constsList.size}")
    constsList.zip(grouped).toMap
  }
}