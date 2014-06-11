package scala.reflect.persistence

class ConstantDecompressor {
  import Enrichments._
  
  def apply(toRead: List[Byte]): Map[String, List[Int]] = {
    val (constSize, xs) = readInt(toRead)
    val constBytes: List[Byte] = xs.take(constSize)
    var rest: List[Byte] = xs.drop(constSize)
    val(occsSize, ys) = readInt(rest)
    rest = ys
    val occs: List[Int] = (0 until occsSize).map{ x => 
      val(entry, zs) = readShort(rest)
      rest = zs
      entry.toInt
    }.toList

    val constsList: List[String] = constBytes.map(_.toChar).mkString.split("\n").toList.sorted
    val grouped: List[List[Int]] = occs.zipWithIndex.groupBy(_._1).toList.sortBy(_._1).map{ x => 
      x._2.map(y => y._2)
    }.toList
    assert(grouped.size == constsList.size)
    constsList.zip(grouped).toMap
  }
}