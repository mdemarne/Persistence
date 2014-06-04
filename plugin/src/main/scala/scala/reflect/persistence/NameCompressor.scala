package scala.reflect.persistence

class NameCompressor {
  import Enrichments._
  
  def apply(names: Map[String, List[Int]]): List[Byte] = {
    val namesOnly = names.keys.toList.sortBy(e => e)
    val namesOccs = names.flatMap(v => v._2 .map(x => (x, v._1)))
    val namesIds = namesOnly.zipWithIndex.toMap
    val maxId = names.values.flatten.max
    val occs = (0 to maxId).map { i =>
    	namesOccs.get(i) match {
        case None => -1 
        case Some(name) => namesIds(name)
      }}.toList
    val v1 = namesOnly.mkString("\n").getBytes().toList 
    val v2 = occs.flatMap(o => ShortToBytes(o.toShort))
    (IntToBytes(v1.size) ::: v1 ::: IntToBytes(v2.size) ::: v2)
  } 
}