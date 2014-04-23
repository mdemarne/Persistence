package scala.reflect.persistence

import java.io.DataInputStream

class AstDecompressor(in: DataInputStream) {
  import Enrichments._

  def rebuiltTree(occs: List[List[NodeBFS]], edges: List[(Int, Int)]): Node = {
    val revOccs = occs.reverse
    val revEdges = edges.reverse
    ???
  }
  
  /* TODO: should be either nested or private. Is public here for tests */
  /* Decode the occurrences from a list of bytes to a list of subtrees in BFS order */
  def decodeOccs(occs: List[Byte], dict: HufDict): List[List[NodeBFS]] = {
    val revDict = dict.map(_.swap)
    def loop(occs: List[Byte]): List[List[NodeBFS]] = occs match {
      case Nil => Nil
      case _ => 
        val (mtch, rest) = revDict.getMatch(occs)
        mtch :: loop(rest)
    }
    loop(occs)
  }
  
  def inputOccs: List[Byte] = ???
  def inputDict: HufDict = ???
  def inputEdges: List[(Int, Int)] = ???

  def apply(): Node = ???

}