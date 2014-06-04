package scala.reflect.persistence

class NameDecompressor {
  import Enrichments._
  
  def apply(toRead: List[Byte]): (Map[String, List[Int]], List[Byte]) = {
    val (size, xs) = readInt(toRead)
   /* var rest: List[Byte] = xs
    val res: Map[String, List[Int]] = (0 until size).map{ y => 
      val name: String = rest.takeWhile(_ != '\n'.toByte).map(_.toChar).mkString
      rest = rest.dropWhile(_ != '\n').tail
      val (entriesSize, xs) = readShort(rest)
      rest = xs
      val entries: List[Int] = (0 until entriesSize).map{ x => 
        val (s, xs) = readShort(rest)
        rest = xs
        s.toInt
      }.toList
      (name -> entries)
    }.toMap
    (res, rest)*/
    val namesByte: List[Byte] = xs.take(size)
    var rest: List[Byte] = xs.drop(size)
    println(s"The rest ${rest}")
    val namesList: List[String] = namesByte.map(_.toChar).mkString.split("\n").toList.sorted
    val (occSize, r) = readInt(rest)
    rest = r
    val occs: List[(Int, Int)] = (0 until occSize).map{ i =>
      println(s"The rest ${rest}")
      val (entry, rs) = readShort(rest)
      println(s"The rest ${rs}")
      rest = rs
      (i, entry.toInt)
    }.toList.filter(_._2 != -1)
    val toZip: List[List[Int]] = occs.groupBy(_._2).toList.map(_._2.map(_._1)).toList
    (namesList.zip(toZip).toMap, rest)
  }
}
