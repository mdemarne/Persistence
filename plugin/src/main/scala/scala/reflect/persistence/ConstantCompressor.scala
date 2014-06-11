package scala.reflect.persistence

class ConstantCompressor{
  import Enrichments._
  
  def apply(constants: Map[String, List[Int]]): List[Byte] = {
    val zipped: List[((String, List[Int]), Int)] = constants.toList.sortBy(_._1).zipWithIndex
    val constOnly = zipped.map(_._1._1)
    val occurrences: List[(Int, Int)] = zipped.map{ x =>
      x._1._2.map(y => (y, x._2))
    }.flatten.sortBy(_._1)
    val constBytes: List[Byte] = constOnly.mkString("\n").getBytes.toList
   IntToBytes(constBytes.size)++ constBytes ++ IntToBytes(occurrences.size) ++ occurrences.map(e => ShortToBytes(e._2.toShort)).flatten 
  } 
}