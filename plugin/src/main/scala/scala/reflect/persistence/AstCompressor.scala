package scala.reflect.persistence
import java.io.DataOutputStream
import scala.annotation.tailrec

class AstCompressor(out: DataOutputStream) extends (Node => Unit) {

  type Dictionary = Map[List[NodeBFS], Int]

  /* TODO: perhaps dict can be a val passed to loop */
  /* TODO: make private; here accessible for testing */
  def parse(root: Node): Dictionary = {
    var dict: Dictionary = Map(root.getSubBFS(1) -> 0)
    @tailrec def loop(que: List[Node]): Unit = que match {
      case Nil => /* Nothing more to parse */
      case nd :: nds =>
        val bfs = nd.childrenBFSIdx
        val candidates = dict.keys.map(cand => bfs.intersectBFS(cand)).filter(_.size > 0)
        val max = candidates./:(List[NodeBFS]())((x,y) => if(x.size > y.size) x else y)
        max.size match {
          case 0 => /* nothing found, we add the new node to dict and all its children to the queue */
            dict += (bfs.takeSubtree(1) -> 1)
            loop(nds ++ nd.children)
          case sz if sz == bfs.size => /* perfect subtree matching */
            dict += (max -> (dict(max) + 1)) /* update the frequency of max in dict */
            loop(nds)
          case sz => /* the match does not cover all the subtree */
            val nwst = bfs.takeSubtree(sz + 1)
            dict += (nwst -> 1) /* add max + one more node */
            candidates foreach (cdt => dict += (cdt -> (dict(cdt) + 1))) /* update counter for all prefix trees */
            loop(nds ++ nwst.subRoots) /* add children of bfs.take(max.size + 1) to que */
        }
    }
    loop(root :: Nil)
    dict
  }
  
  private def encode(dict: Dictionary): Unit = { /* TODO */ }

  implicit class ListNodeBFS(lst: List[NodeBFS]) {
    /* Return a common subtree of this and n if exists, with the size of the subtree in BFS order */
    def intersectBFS(nds: List[NodeBFS]): List[NodeBFS] = {
      def loop(nds1: List[NodeBFS], nds2: List[NodeBFS]): List[NodeBFS] = (nds1, nds2) match {
        case (x :: xs, y :: ys) if x :=: y => y :: loop(xs, ys)
        case _ => Nil
      }
      val inter = loop(lst.reverse, nds.reverse).reverse
      if (inter.size < nds.size) Nil 
      else inter
    }
    /* Return all the subroots of a tree represented as a List[NodeBFS], as a List[Node] */
    def subRoots: List[Node] = {
      val nodes = lst.map(_.node)
      val children = nodes.flatMap(nd => nd.children.reverse) /* Get all the children from the nodes in lst */
      children.filter(nd => !nodes.contains(nd)).reverse
    }
     /* TODO: a lot of similar methods all around (ex. in Node, ListTakeAndSplit in plugin.scala, etc. Perhaps exists a way to clean that up. */
    def takeSubtree(i: Int) = lst.reverse.take(i).reverse
  }

  /* Generates the dictionary for this tree */
  def apply(node: Node): Unit = { /* TODO */ }
}
