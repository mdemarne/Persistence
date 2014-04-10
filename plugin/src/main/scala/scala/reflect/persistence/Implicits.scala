package scala.reflect.persistence

import scala.annotation.tailrec

/* Regroup all the implicit classes used a bit everywhere in the plugin */
object Implicits {

  type RevList[A] = List[A] /* leaves first */

  implicit class BFSListToBFSMapWithIndxs[T](lst: RevList[T]) {
    /* Generate a map of (T, List[Int]), where the values are the occurrences of T in the tree in BFS order */
    def zipWithIdxs: Map[T, List[Int]] = lst.zipWithIndex.groupBy(v => v._1).map(e => (e._1 -> e._2.map(i => i._2)))
  }

  implicit class BFSMapWithIndxToBFSList[T](lst: Map[T, List[Int]]) {
    /* Generate a list of (T) following the indexes in the given Map[T, List[Int]] */
    def unzipWithIdxs: RevList[T] = lst.flatMap(el => el._2 map (indx => (indx, el._1))).toList.sortBy(f => f._1).map(_._2)
  }

  implicit class ListNodeBFS(lst: RevList[NodeBFS]) {
    /* Return a common subtree of this and n if exists, with the size of the subtree in BFS order */
    def intersectBFS(nds: RevList[NodeBFS]): RevList[NodeBFS] = {
      def loop(nds1: RevList[NodeBFS], nds2: RevList[NodeBFS]): RevList[NodeBFS] = (nds1, nds2) match {
        case (x :: xs, y :: ys) if x == y => y :: loop(xs, ys)
        case _ => Nil
      }
      val inter = loop(lst.reverse, nds.reverse).reverse
      if (inter.size < nds.size) Nil
      else inter
    }
    /* Return all the subroots of a tree represented as a List[NodeBFS], as a List[Node] */
    def subRoots: List[Node] = {
      val bfsz = lst.map(x => (x, lst.count(z => z.parentBfsIdx == x.bfsIdx)))
      bfsz.flatMap(x => x._1.node.children.drop(x._2))
    }
    def takeSubtree(i: Int) = lst.reverse.take(i).reverse

    /* TODO never called, might be useful later */
    /* From a list of NodeBFS, return a reconstructed tree. The NodeBFS must be well formated */
    def toTree: Option[Node] = {
      /* Recursively goes through the old list of NodeBFS to reconstruct a list of NodeBFS where each 
       * Node contains only the children present in the subtree represented by the nodes of the old list. */
      @tailrec def loop(old: RevList[NodeBFS], nw: RevList[NodeBFS]): List[NodeBFS] = old match {
        case x :: xs =>
          loop(xs, nw.map(p =>
            if (p.bfsIdx == x.parentBfsIdx)
              p.addChild(nw.find(_.bfsIdx == x.bfsIdx).get.node)
            else p))
        case Nil => nw /* at the end, each node in nw has been updated */
      }
      Some(loop(lst, lst.map(n => n.copy(node = n.node.cleanCopy))).last.node)
    }
  }
  
  implicit class ListTakeAndSplit[T](lst: List[T]) {
    def firsts = lst.reverse.tail.reverse
    def takeWithoutLasts(n: Int) = lst.reverse.drop(n).reverse
    def splitOn(p: T => Boolean): List[List[T]] = {
      def loop(xss: List[T]): List[List[T]] = xss match {
        case Nil => Nil
        case _ =>
          val span = xss span (x => !p(x))
          List(span._1) ++ (if (span._2 != Nil) loop(span._2.tail) else Nil)
      }
      loop(lst)
    }
  }
}
