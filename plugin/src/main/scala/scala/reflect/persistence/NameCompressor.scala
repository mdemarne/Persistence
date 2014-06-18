/**
 * Compress names to bytes. The name's string are compressed first, and then
 * their occurrences based on ordered IDs.
 *
 * @author Mathieu Demarne, Adrien Ghosn
 */
package scala.reflect.persistence

class NameCompressor {
  import Enrichments._

  def apply(names: Map[String, List[Int]]): List[Byte] = {
    val zipped: List[((String, List[Int]), Int)] = names.toList.sortBy(_._1).zipWithIndex
    val namesOnly = zipped.map(_._1._1)
    val occurrences: List[(Int, Int)] = zipped.map(x => x._1._2.map(y => (y, x._2))).flatten.sortBy(_._1)
    val nameBytes: List[Byte] = namesOnly.mkString("\n").getBytes.toList
    IntToBytes(nameBytes.size) ++ nameBytes ++ IntToBytes(occurrences.size) ++ occurrences.map(e => ShortToBytes(e._2.toShort)).flatten
  }
}
