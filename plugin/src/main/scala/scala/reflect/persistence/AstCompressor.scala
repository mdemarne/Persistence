package scala.reflect.persistence

import java.io.DataOutputStream
import scala.annotation.tailrec
import scala.language.postfixOps

/* TODO: optimize. The splitTree method is very slow on big files, (ex. Typers.scala, 5'500 lines of code) */
/* TODO: make some functions private. Here public for tests */
class AstCompressor(out: DataOutputStream) {
  import Enrichments._

  /* Reparse the tree using the new dictionary */
  /* TODO: should be either nested or private. Is public here for tests */
  def splitTree(node: Node): (NodeDict, List[List[NodeBFS]], List[(Int, Int)]) = {
    val ids = {
      def idsc(v: Int): Stream[Int] = v #:: idsc(v + 1)
      idsc(0).iterator
    }
    val freqList = node.computeFreqs.toList
    val exponent = freqList.map(_._1.size).sum.toDouble / freqList.size - 1
    val keyList = freqList
      .filter(entry => entry._1.size < Math.sqrt(node.flattenBFS.size))
      .map(entry => (entry._1, Math.pow(entry._1.size, exponent) * entry._2))
      .sortBy(entry => (entry._2, entry._1.size)).reverse
      .map(entry => entry._1).toList
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
    out.writeShort(occs.size)
    out.write(compressBytes(occs))
    out.flush
  }
  def outputDict(dict: HufDict): Unit = {
    out.writeInt(dict.size)
    dict.foreach { e =>
      out.writeInt(e._2.size)
      out.write(compressBytes(e._2))
      val ndBfs = e._1.asPrintable
      out.writeShort(ndBfs.size)
      ndBfs.foreach { n => out.write(n._1); out.writeShort(n._2); out.writeShort(n._3) }
    }
    out.flush
  }
  def outputEdges(edges: List[(Int, Int)]): Unit = {
    out.writeInt(edges.size - 1)
    edges.tail foreach { edge =>
      out.writeShort(edge._1)
      out.writeShort(edge._2)
    }
    out.flush
  }
  //Compresses the List of 0 and 1's into bytes
  def compressBytes(bytes: List[Byte]): Array[Byte] = {
    val groups: List[(Int, List[(Byte, Int)])] = bytes.reverse.zipWithIndex.groupBy(_._2 / 8).toList
    val octoBytes: List[List[Byte]] = groups.sortBy(i => i._1).map(l => l._2.map(_._1))
    octoBytes.map { o =>
      o.zipWithIndex.map(b => (b._1 << b._2).toByte).sum.toByte
    }.toArray
  }
  def apply(node: Node): Unit = {
    val (nodeDict, occs, edges) = splitTree(node)
    val hufDict = genHuffman(nodeDict)
    val encodedOccs = encodeOccs(occs, hufDict)
    outputOccs(encodedOccs)
    outputCompEdges(edges)
    outputDict(hufDict)
  }

  def outputCompEdges(edges: List[(Int, Int)]): Unit = {
    @tailrec def loop(old: (Int, Int), count: Int, edgs: List[(Int, Int)]): Unit = edgs match {
      case Nil =>
        out.writeShort(count)
      case x :: xs if old == x =>
        loop(old, count + 1, xs)
      case x :: xs =>
        out.writeShort(count)
        out.writeShort(x._1)
        out.writeShort(x._2)
        loop(x, 1, xs)
    }
    out.writeInt(edges.tail.size)
    out.writeShort(edges.tail.head._1)
    out.writeShort(edges.tail.head._2)
    loop(edges.tail.head, 1, edges.tail.tail)

    /*def loop(edg: List[(Int, Int)]): List[((Int,Int), Int)] = edg match {
      case x::xs =>
        val numb = xs.takeWhile(_ == x).size
        (x, numb + 1)::loop(edg.dropWhile(_ == x))
      case Nil => Nil
    }
    val compr = loop(edges.tail)
    out.writeInt(compr.size)
    compr.foreach{  e =>
      out.writeShort(e._1._1); out.writeShort(e._1._2)
      out.writeShort(e._2)
    }*/
    out.flush
  }

  def outputComp2Edges(edges: List[(Int, Int)]): Unit = {
    val (lp1, lp2) = edges.tail.unzip
    out.writeInt(lp1.size)
    def loop (l: List[Int]): List[(Int, Int)] = l match{
      case x::xs => 
        val numb = xs.takeWhile(_ == x).size
        (x, numb + 1)::loop(l.dropWhile(_ == x))
      case Nil => Nil
    }
    val inter = loop(lp1)
    val lp11 = inter.tail.zip(inter).map(e => ((e._1._1 - e._2._1), e._1._2))
    val lp22 = loop(lp2)
    out.writeInt(lp11.size -1)
    out.writeInt(lp22.size)
    out.writeShort(inter.head._1); out.writeShort(inter.head._2)
    lp11.foreach{e => out.writeByte(e._1.toByte); out.writeShort(e._2)}
    lp22.foreach{e => out.writeShort(e._1); out.writeShort(e._2)}
    out.flush
  }
}