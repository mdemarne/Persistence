package scala.reflect.persistence

import java.io.DataOutputStream
import scala.annotation.tailrec
import scala.language.postfixOps
import org.tukaani.xz.LZMA2InputStream
import java.io.{File,FileInputStream,FileOutputStream}
import org.tukaani.xz.{XZOutputStream, LZMA2Options}
import java.io.OutputStream

/* TODO: make some functions private. Here public for tests */
class AstCompressor {
  import Enrichments._
  
  var toWrite: List[Byte] = Nil
 
  /* Reparse the tree using the new dictionary */
  def splitTree(node: Node): (NodeDict, List[List[NodeBFS]], List[(Int, Int)]) = {
    val ids = {
      def idsc(v: Int): Stream[Int] = v #:: idsc(v + 1)
      idsc(0).iterator
    }
    val keyList = { 
    val freqs = node.computeFreqs.toList
    val metric = Math.sqrt(node.flattenBFS.size)
    val filtered = freqs.filter(entry => entry._1.size < metric)
    val mapped =  filtered.map(entry => (entry._1, Math.pow(entry._1.size, 1) * entry._2))
    //TODO a bit long
    mapped.sortBy(entry => (entry._2, entry._1.size)).reverse
      .map(entry => entry._1).toList}
    /* origin dictionary, with empty frequencies */
    val originDict: NodeDict = keyList.map(k => (k, 0)) toMap
    @tailrec def loop(que: List[(Node, Int, Int, Int)], dict: NodeDict, occ: List[List[NodeBFS]], edges: List[(Int, Int)]): (NodeDict, List[List[NodeBFS]], List[(Int, Int)]) = que match {
      case Nil => (dict, occ, edges)
      case nd :: nds =>
        val bfs = nd._1.childrenBFSIdx
        keyList.find(entry => entry.matchBFS(bfs)) match {
          case None => sys.error("Cannot find matching entry in the dictionary")
          case Some(entry) =>
            /* node, parsing index, index of the node in BFS in the tree corresponding to the index to which the subroot was linked */
            val subRoots = bfs.intersectBFS(entry).subRoots.map(s => (s._1, nd._4, s._2, ids.next))
            loop(nds ++ subRoots,
              dict + (entry -> (dict(entry) + 1)),
              occ :+ entry,
              edges :+ (nd._2, nd._3))
        }
    }
    val (dict, occ, edges) = loop((node, -1, -1, ids.next) :: Nil, originDict, Nil, Nil)
    (dict.filter(entry => entry._2 > 0), occ, edges)
  }
  def genHuffman(dict: NodeDict): HufDict = {
    trait HufTree { val freq: Int }
    case class HufLeaf(key: List[NodeBFS], freq: Int) extends HufTree
    case class HufNode(freq: Int, left: HufTree, right: HufTree) extends HufTree
    @tailrec def computeHufTree(que: List[HufTree]): HufTree = que match {
      case Nil => sys.error("Error in Huffman tree generation ")
      case x :: Nil => x
      case x :: y :: xs => computeHufTree((xs :+ HufNode(x.freq + y.freq, x, y)).sortBy(_.freq))
    }
    def computeHufValues(hufTree: HufTree, cde: List[Byte] = Nil): List[(List[NodeBFS], List[Byte])] = hufTree match {
      case HufLeaf(key, _) => (key, cde) :: Nil
      case HufNode(_, left, right) => computeHufValues(left, cde :+ 0x1.toByte) ++ computeHufValues(right, cde :+ 0x0.toByte)
    }
    val hufQueue: List[HufTree] = dict.toList.map(entry => HufLeaf(entry._1, entry._2))
    computeHufTree(hufQueue) match {
      case HufLeaf(key, _) => ((key, 0x1.toByte :: Nil) :: Nil) toMap /* Corner case: if only one entry in dict */
      case _ => computeHufValues(computeHufTree(hufQueue)) toMap
    }
  }
  /* Once the Huffman code generated based on the frequencies found while reparsing the tree, encode the list of occurences. */
  def encodeOccs(occ: List[List[NodeBFS]], dict: HufDict): List[Byte] = {
    def loop(occ: List[List[NodeBFS]], set: List[Byte]): List[Byte] = occ match {
      case Nil => set
      case x :: xs => loop(xs, set ++ dict(x))
    }
    loop(occ, Nil)
  }
  
  def outputOccs(occs: List[Byte]): Unit = {
    toWrite ++= ShortToBytes(occs.size.toShort)
    toWrite ++= compressBytes(occs)
  }
  def outputDict(dict: HufDict): Unit = {
    toWrite ++= IntToBytes(dict.size)
    dict.foreach { e =>
      toWrite ++= IntToBytes(e._2.size)
      toWrite ++= compressBytes(e._2)
      val ndBfs = e._1.asPrintable
      toWrite ++= ShortToBytes(ndBfs.size.toShort)
      ndBfs.foreach { n => toWrite :+= (n._1); toWrite ++= ShortToBytes(n._2.toShort); toWrite ++= ShortToBytes(n._3.toShort)}
    }
  }
  def outputEdges(edges: List[(Int, Int)]): Unit = {
    require(edges.size > 0)
    val (lp1, lp2) = edges.tail.unzip
    @tailrec def loop(curr: Int, count: Int, entries: List[Int], bool: Boolean): Unit = entries match {
      case Nil => 
        toWrite ++= ShortToBytes(count.toShort)
      case x::xs if x == curr =>
        loop(curr, count + 1, xs, bool)
      case x::xs =>
        toWrite ++= ShortToBytes(count.toShort)
        if(bool) 
          toWrite :+= (x-curr).toByte
        else 
          toWrite ++= ShortToBytes(x.toShort)
        loop(x, 1, xs, bool)
    }
    toWrite ++= IntToBytes(lp1.size)
    toWrite ++= ShortToBytes(lp1.head.toShort)
    loop(lp1.head, 1, lp1.tail, true)
    toWrite ++= ShortToBytes(lp2.head.toShort)
    loop(lp2.head, 1, lp2.tail, false)
  }
  /* Compresses the List of 0 and 1's into bytes */
  def compressBytes(bytes: List[Byte]): Array[Byte] = {
    val groups: List[(Int, List[(Byte, Int)])] = bytes.reverse.zipWithIndex.groupBy(_._2 / 8).toList
    val octoBytes: List[List[Byte]] = groups.sortBy(i => i._1).map(l => l._2.map(_._1))
    octoBytes.map { o =>
      o.zipWithIndex.map(b => (b._1 << b._2).toByte).sum.toByte
    }.toArray
  }
  
  def apply(node: Node): List[Byte] = {
    val (nodeDict, occs, edges) = splitTree(node)
    val hufDict = genHuffman(nodeDict)
    val encodedOccs = encodeOccs(occs, hufDict)
    outputOccs(encodedOccs)
    outputEdges(edges)
    outputDict(hufDict)
    toWrite
  }
}
