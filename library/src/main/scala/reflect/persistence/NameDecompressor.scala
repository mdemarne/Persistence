package scala.reflect.persistence

class NameDecompressor {
  import Enrichments._
  
  def apply(toRead: List[Byte]): (Map[String, List[Int]], List[Byte]) = {
    val (size, xs) = readInt(toRead)
    var rest: List[Byte] = xs
    val res: Map[String, List[Int]] = (0 until size).map{ y => 
      val name: String = rest.takeWhile(_ != '\n'.toByte).map(_.toChar).mkString
      rest = toRead.dropWhile(_ != '\n').tail
      val (entriesSize, xs) = readShort(rest)
      rest = xs
      val entries: List[Int] = (0 until entriesSize).map{ x => 
        val (s, xs) = readShort(rest)
        rest = xs
        s.toInt
      }.toList
      (name -> entries)
    }.toMap
    (res, rest)
  }
}
