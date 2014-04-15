package scala.reflect.persistence

import java.io.DataOutputStream
import scala.annotation.tailrec

class AstCompressor(out: DataOutputStream) {
  import Enrichments._

  def encodeDict(dict: NodeDict): String = ???

  def apply(node: Node): Unit = {
    /* First, we filter the dictionary in a proper way */
    val keyList = node.computeFreqs.toList
      .filter(entry => entry._1.size < Math.sqrt(node.flattenBFS.size))
      .map(entry => (entry._1, entry._1.size * entry._2))
      .sortBy(entry => (entry._2, entry._1.size))
      .map(entry => entry._1).toList

    /* Then we reparse the tree using the new dictionary */

    def encodeTree(node: Node, originDict: NodeDict): (String, NodeDict) = {
      def loop(que: List[Node], dict: NodeDict, out: List[List[NodeBFS]], edges: List[Int]): (NodeDict, List[List[NodeBFS]], List[Int]) = que match {
        case Nil => (dict, out, edges)
        case nd :: nds => ???
        /*val bfs = nd.childrenBFSIdx
	        val mtch = keyList.find(entry => bfs.intersectBFS(entry).size > 0)
	        mtch match {
	          case None => sys.error("Cannot find matching entry in the dictionary")
	          case Some(entry) => ??? /* TODO */
	        		 loop(nds + mtch.subRoots,(entry -> (dict(entry) +1)))
	        }
	    loop(node :: Nil, originDict)*/
      }
      ???
    }
    ???
  }
}