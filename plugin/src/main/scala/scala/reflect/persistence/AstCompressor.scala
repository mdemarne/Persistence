package scala.reflect.persistence
import java.io.DataOutputStream
import scala.annotation.tailrec

class AstCompressor(out: DataOutputStream) extends (Node => Unit) {
  import Implicits._

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
        val max = candidates./:(List[NodeBFS]())((x, y) => if (x.size > y.size) x else y)
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

  /* Generates the dictionary for this tree */
  def apply(node: Node): Unit = { /* TODO */ }
}
