/** Compress constants to bytes while keeping their types.
 *
 * @author Mathieu Demarne, Adrien Ghosn
 */
package scala.reflect.persistence

class ConstantCompressor[U <: scala.reflect.api.Universe](val u: U) {
  import Enrichments._
  import u._

  def apply(constants: Map[Any, List[Int]]): List[Byte] = {
    val constantsAsList = constants.toList
    val zipped = (constantsAsList.filter(_._1 != null).sortBy(_._1.toString) ++ constantsAsList.filter(_._1 == null)).zipWithIndex
    val constOnly = zipped.map(_._1._1)
    val occurrences: List[(Int, Int)] = zipped.map(x => x._1._2.map(y => (y, x._2))).flatten.sortBy(_._1)
    val constBytes: List[Byte] = constOnly.flatMap(_ match {
      case v: Boolean => 'B'.toByte :: (if (v) 1.toByte else 0.toByte) :: Nil
      case v: Byte => 'b'.toByte :: v :: Nil
      case v: Short => 's'.toByte :: ShortToBytes(v)
      case v: Char => 'c'.toByte :: v.toByte :: Nil
      case v: Int => 'i'.toByte :: IntToBytes(v)
      case v: Long => 'l'.toByte :: LongToBytes(v)
      case v: Float => 'f'.toByte :: FloatToBytes(v)
      case v: Double => 'd'.toByte :: DoubleToBytes(v)
      case v: String => 'g'.toByte :: ShortToBytes(v.size.toShort) ++ v.getBytes /* For strings, the size is written as a short */
      /* TODO: 	For now, Types and Symbols are simply transformed into String, due to the difference with Palladium Trees */
      case v: Type =>
        val str = v.toString
        'T'.toByte :: ShortToBytes(str.size.toShort) ++ str.getBytes
      case v: Symbol =>
        val str = v.toString
        'S'.toByte :: ShortToBytes(str.size.toShort) ++ str.getBytes
      case _ => 'e'.toByte :: Nil
    })
    IntToBytes(constBytes.size) ++ constBytes ++ IntToBytes(occurrences.size) ++ occurrences.map(e => ShortToBytes(e._2.toShort)).flatten
  }
}