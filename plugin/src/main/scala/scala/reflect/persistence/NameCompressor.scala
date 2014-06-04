package scala.reflect.persistence

class NameCompressor {
  import Enrichments._
  
  def apply(names: Map[String, List[Int]]): List[Byte] = {
   val (namesOnly, occs) = names.toList.sortBy(_._1).unzip 
  } 
}
