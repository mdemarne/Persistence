package scala.reflect.persistence

import java.io.DataOutputStream
import scala.annotation.tailrec
import scala.language.postfixOps

class AstCompressor(out: DataOutputStream) {
  import Enrichments._


  /* Reparse the tree using the new dictionary */
  def splitTree(node: Node): (NodeDict, List[List[NodeBFS]], List[Int]) = {
    val keyList = node.computeFreqs.toList
      .filter(entry => entry._1.size < Math.sqrt(node.flattenBFS.size))
      .map(entry => (entry._1, entry._1.size * entry._2))
      .sortBy(entry => (entry._2, entry._1.size))
      .map(entry => entry._1).toList
    /* origin dictionary, with empty frequencies */
    val originDict: NodeDict = keyList.map(k => (k, 0)) toMap
    def loop(que: List[Node], dict: NodeDict, occ: List[List[NodeBFS]], edges: List[Int]): (NodeDict, List[List[NodeBFS]], List[Int]) = que match {
      case Nil => (dict, occ, edges)
      case nd :: nds =>
        val bfs = nd.childrenBFSIdx
        keyList.find(entry => entry.matchBFS(bfs)) match {
          case None => sys.error("Cannot find matching entry in the dictionary")
          case Some(entry) =>
            val subtree = bfs.intersectBFS(entry)
            loop(nds ++ subtree.subRoots,
              dict + (entry -> (dict(entry) + 1)),
              occ :+ entry,
              edges :+ 0) /* TODO */
        }
    }
    val (dict, occ, edges) = loop(node :: Nil, originDict, Nil, Nil)
    (dict.filter(entry => entry._2 > 0), occ, edges)
  }

  def encodeDict(dict: NodeDict): String = ???

  def apply(node: Node): Unit = ???
}