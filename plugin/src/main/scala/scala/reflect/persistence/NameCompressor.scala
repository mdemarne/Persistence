package scala.reflect.persistence

class NameCompressor {
  import Enrichments._
  
  def apply(names: Map[String, List[Int]]): List[Byte] = {
    IntToBytes(names.size) ++ names.flatMap{ n =>
      n._1.getBytes.toList ++ ('\n'.toByte :: Nil) ++ ShortToBytes(n._2.size.toShort) ++ n._2.map(e => ShortToBytes(e.toShort)).flatten 
    }
  } 
}