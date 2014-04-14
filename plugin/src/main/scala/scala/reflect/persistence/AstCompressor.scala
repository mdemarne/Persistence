package scala.reflect.persistence

import java.io.DataOutputStream
import scala.annotation.tailrec

class AstCompressor(out: DataOutputStream) {
  import Enrichments._
  
    type OrderedNodeDict = List[(List[NodeBFS], Int)] /* Represent a compression dictionary for trees with weigths and frequencies */

  def encodeTree(node: Node, originDict: OrderedNodeDict): (String, OrderedNodeDict) = {
    var dict: OrderedNodeDict = originDict
    var output: String = ""
    @tailrec def loop(que: List[Node]): Unit = que match {
      case Nil => /* Nothing more to parse */
      case nd :: nds =>
        val bfs = nd.childrenBFSIdx
        val mtch = dict.find(entry => bfs.intersectBFS(entry._1).size > 0)
        mtch match {
          case None => sys.error("Cannot find matching entry in the dictionary")
          case Some(entry) => ??? /* TODO */
        }        
    } 
    loop(node :: Nil)
    (output, dict)
  }
  
  def encodeDict(dict: NodeDict): String = ???
  
  def apply(node: Node): Unit = {
    /* First, we filter the dictionary in a proper way */
    val dict: OrderedNodeDict = node.computeFreqs.toList
      .filter(entry => entry._1.size < Math.sqrt(node.flattenBFS.size))
      .map(entry => (entry._1, entry._1.size * entry._2))
      .sortBy(entry => (entry._2, entry._1.size))
      .map(entry => (entry._1, 0)).toList

    /* Then we reparse the tree using the new dictionary */
    
  }
}