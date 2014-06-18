/**
 * General enrichments for lists, Nodes, dictionaries.
 *
 * @author Mathieu Demarne, Adrien Ghosn
 */
package scala.reflect.persistence

import scala.annotation.tailrec
import java.nio.ByteBuffer

object Enrichments {

  type RevList[A] = List[A] /* leaves first */
  type NodeDict = Map[List[NodeBFS], Int] /* Represent a compression dictionary for trees of nodes with frequencies */
  type HufDict = Map[List[NodeBFS], List[Byte]]
  type RevHufDict = Map[List[Byte], List[NodeBFS]]

  /* Meta entry for dictionary testing */
  case class MetaEntry(tpe: NodeTag.Value, idx: Int, parentIdx: Int) {
    override def toString = s"(${tpe}, ${idx}, ${parentIdx})"
  }

  implicit class RichNodeDict(dict: NodeDict) {
    /* Print a dictionary as MetaEntries. Used for testing */
    def testingDict = dict map (x => (x._1.map(y => MetaEntry(y.node.tpe, y.bfsIdx, y.parentBfsIdx)), x._2))
  }

  implicit class RichRevList[T](lst: RevList[T]) {
    /* Generate a map of (T, List[Int]), where the values are the occurrences of T in the tree in BFS order */
    def zipWithIdxs: Map[T, List[Int]] = lst.zipWithIndex.groupBy(v => v._1).map(e => (e._1 -> e._2.map(i => i._2)))
  }

  implicit class RichMap[T](lst: Map[T, List[Int]]) {
    /* Generate a map of (Int -> T) following the values in the given Map[T, List[Int]] */
    def unzipWithIdxs: Map[Int, T] = lst.flatMap(el => el._2 map (indx => (indx, el._1))).toMap
  }

  implicit class RichRevListNodeBFS(lst: RevList[NodeBFS]) {

    /* Return a common subtree of this and n if exists, with the size of the subtree in BFS order */
    def intersectBFS(nds: RevList[NodeBFS]): RevList[NodeBFS] = {
      def loop(nds1: RevList[NodeBFS], nds2: RevList[NodeBFS]): RevList[NodeBFS] = (nds1, nds2) match {
        case (x :: xs, y :: ys) if x.equalsStructure(y) => x :: loop(xs, ys)
        case _ => Nil
      }
      loop(lst.reverse, nds.reverse).reverse
    }

    /* true if lst match perfecly nds */
    def matchBFS(nds: RevList[NodeBFS]) = intersectBFS(nds).size == lst.size

    /* Return all the subroots of a tree represented as a List[NodeBFS], as a RevList[Node], along with the node ID in BFS order from which it was linked */
    def subRoots: RevList[(Node, Int)] = {
      val bfsz = lst.map(x => (x, lst.count(z => z.parentBfsIdx == x.bfsIdx)))
      bfsz.flatMap(x => (x._1.node.children.drop(x._2).map(c => (c, x._1.bfsIdx)).reverse))
    }
    def takeSubtree(i: Int) = lst.takeRight(i)

    /* From a list of NodeBFS, return a reconstructed tree. The NodeBFS must be well formated */
    def toTree: Node = {
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
      loop(lst, lst.map(n => n.copy(node = n.node.cleanCopy))).last.node
    }

    /* Append that to this at position parentBFSIdx in this */
    def append(that: RevList[NodeBFS], parentBFSIdx: Int): RevList[NodeBFS] = {
      val tree = lst.toTree
      val oldParent = tree.flattenBFSIdx.reverse(parentBFSIdx).node
      /* add the subtree to its parent */
      val newParent = oldParent.copy(children = oldParent.children :+ that.toTree)
      /* Recursively copy it back in the tree */
      def addChildren(subtree: Node): Node = {
        if (oldParent eq subtree) newParent /* Use of eq to compare by reference */
        else if (subtree.children.isEmpty) subtree
        else subtree.copy(children = subtree.children.map(c => addChildren(c)))
      }
      addChildren(tree).flattenBFSIdx
    }

    /* Used for testing */
    def asPrintable: List[(Byte, Short, Short)] = lst.map { e => (NodeTag.getIndex(e.node.tpe).toByte, e.bfsIdx.toShort, e.parentBfsIdx.toShort) }.toList
  }

  implicit class RichList[T](lst: List[T]) {
    /* Split the list in sublists on p while removing T corresponding to p */
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

  implicit class RichRevHufDict(dict: RevHufDict) {
    /* Find the matching entry in the dictionary from bytes to List[NodeBFS] based on the prefix of bytes. The rest is returned as well */
    def getMatch(in: List[Byte]): (List[NodeBFS], List[Byte]) = {
      @tailrec def loop(x: List[Byte], xs: List[Byte]): (List[NodeBFS], List[Byte]) = dict.get(x) match {
        case Some(entry) => (entry, xs)
        case None => loop(x :+ xs.head, xs.tail)
      }
      loop(in.head :: Nil, in.tail)
    }
  }

  /* Warning: must give the list of nodes in normal BFS and head is the node we need */
  def extractSubBFS(nodes: List[NodeBFS]): RevList[NodeBFS] = {
    assert(!nodes.isEmpty)
    def loop(nds: List[NodeBFS], acc: RevList[NodeBFS]): RevList[NodeBFS] = nds match {
      case Nil =>
        acc
      case n :: ns if (acc.head.bfsIdx > n.bfsIdx && !acc.exists(_.bfsIdx == n.parentBfsIdx)) =>
        acc
      case n :: ns if (acc.exists(x => x.bfsIdx == n.parentBfsIdx)) =>
        loop(ns, n :: acc)
      case n :: ns =>
        loop(ns, acc)
    }
    loop(nodes.tail, nodes.head :: Nil)
  }

  /* Conversion functions used for compression and decompression */
  def IntToBytes(i: Int): List[Byte] = ByteBuffer.allocate(4).putInt(i).array.toList
  def ShortToBytes(s: Short): List[Byte] = ByteBuffer.allocate(2).putShort(s).array.toList
  def LongToBytes(l: Long): List[Byte] = ByteBuffer.allocate(8).putLong(l).array.toList
  def FloatToBytes(f: Float): List[Byte] = ByteBuffer.allocate(4).putFloat(f).array.toList
  def DoubleToBytes(d: Double): List[Byte] = ByteBuffer.allocate(8).putDouble(d).array.toList
  def readInt(toRead: List[Byte]): (Int, List[Byte]) = (ByteBuffer.wrap(toRead.take(4).toArray).getInt, toRead.drop(4))
  def readShort(toRead: List[Byte]): (Short, List[Byte]) = (ByteBuffer.wrap(toRead.take(2).toArray).getShort, toRead.drop(2))
  def readLong(toRead: List[Byte]): (Long, List[Byte]) = (ByteBuffer.wrap(toRead.take(8).toArray).getLong, toRead.drop(8))
  def readFloat(toRead: List[Byte]): (Float, List[Byte]) = (ByteBuffer.wrap(toRead.take(4).toArray).getFloat, toRead.drop(4))
  def readDouble(toRead: List[Byte]): (Double, List[Byte]) = (ByteBuffer.wrap(toRead.take(8).toArray).getDouble, toRead.drop(8))

  /* Decompresses the byte into a list of 8 bytes */
  def decompressBytes(byte: Byte): List[Byte] = (0 to 7).map { i => if ((byte & (1 << i)) != 0) 1.toByte else 0.toByte }.toList.reverse
}
