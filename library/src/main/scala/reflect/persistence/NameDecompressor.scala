package scala.reflect.persistence

class NameDecompressor {
  import Enrichments._
  
  def apply(toRead: List[Byte]): (Map[String, List[Int]], List[Byte]) = {
    val (nameSize, xs) = readInt(toRead)
    val nameBytes: List[Byte] = xs.take(nameSize)
    var rest: List[Byte] = xs.drop(nameSize)
    val(occsSize, ys) = readInt(rest)
    rest = ys
    val occs: List[Int] = (0 until occsSize).map{ x => 
      val(entry, zs) = readShort(rest)
      rest = zs
      entry.toInt
    }.toList
    val namesList: List[String] = nameBytes.map(_.toChar).mkString.split("\n").toList
    val grouped: List[List[Int]] = occs.zipWithIndex.groupBy(_._1).toList.sortBy(_._1).map{ x => 
      x._2.map(y => y._2)
    }.toList
    assert(grouped.size == namesList.size)
    (namesList.zip(grouped).toMap, rest)
  }
}
