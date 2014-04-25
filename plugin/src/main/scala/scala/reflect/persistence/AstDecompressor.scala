package scala.reflect.persistence

import java.io.DataInputStream

class AstDecompressor(in: DataInputStream) {
  import Enrichments._

  /* TODO: should be either nested or private. Is public here for tests */
  def rebuiltTree(occs: List[List[NodeBFS]], edges: List[(Int, Int)]): Node = {
    def loop(revOccs: RevList[List[NodeBFS]], revEdges: RevList[(Int, Int)]): List[NodeBFS] = (revOccs, revEdges) match {
      case (x :: Nil, Nil) => x /* We have recomposed all the tree */
      case (x :: xs, (idx, parentBFS) :: ys) =>
        val (bef, parent :: aft) = revOccs.splitAt(revOccs.size - idx)
        loop(bef ++ (parent.append(x, parentBFS) :: aft), ys)
        
      case _ => sys.error("Mismatch bettwen the number of occurences and edges.")
        
    } 
    loop(occs.reverse, edges.reverse).toTree.get
  }
  
  /* TODO: should be either nested or private. Is public here for tests */
  /* Decode the occurrences from a list of bytes to a list of subtrees in BFS order */
  def decodeOccs(occs: List[Byte], revDict: RevHufDict): List[List[NodeBFS]] = {
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