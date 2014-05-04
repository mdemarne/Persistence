package scala.reflect.persistence

import java.io.DataInputStream

/* TODO: make some functions private. Here public for tests */
class AstDecompressor(in: DataInputStream) {
  import Enrichments._

  def rebuiltTree(occs: List[List[NodeBFS]], edges: List[(Int, Int)]): Node = {
    def loop(revOccs: RevList[List[NodeBFS]], revEdges: RevList[(Int, Int)]): List[NodeBFS] = (revOccs, revEdges) match {
      case (x :: Nil, Nil) => x /* We have recomposed all the tree. NB: (-1, -1) for the root should not be there. */
      case (x :: xs, (idx, parentBFS) :: ys) =>
        val (bef :+ parent, aft) = xs.splitAt(xs.size - idx)
        loop(bef ++ (parent.append(x, parentBFS) :: aft), ys)
        
      case _ => sys.error("Mismatch bettwen the number of occurences and edges.")
    } 
    loop(occs.reverse, edges.reverse).toTree
  }
  
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
 
  //TODO check that it is better than while hasNext
  def inputOccs: List[Byte] = readBytes(in.readShort)

  def inputDict: RevHufDict = {
    var dict: RevHufDict = Map()
    val nbEntries = in.readInt
    (0 until nbEntries).foreach{ i => 
      val sizeHuff: Int = in.readInt
      val huffcode = readBytes(sizeHuff)
      val nbNode: Int = in.readShort
      val ndBfs: List[NodeBFS] = 
        (for(i <- 1 to nbNode) 
          yield (NodeTag.getVal(in.readByte.toInt), in.readShort.toInt, in.readShort.toInt)).toList.map(i => NodeBFS(Node(i._1, Nil), i._2, i._3))
      dict += (huffcode -> ndBfs)
    }
    dict
  }

  def readBytes(size: Int): List[Byte] = {
    var bytes: List[Byte] = Nil
    for(i <- (0 until Math.ceil(size.toDouble / 8).toInt)){
      bytes  :+= in.readByte
    }
    bytes.map(decompressBytes(_)).reverse.flatten.drop((8 - (size % 8)) % 8)
  }

  def inputEdges: List[(Int, Int)] = {
    val size: Int = in.readInt
    (for(i <- 1 to size) yield (in.readShort.toInt, in.readShort.toInt)).toList
  }

  //Decompresses the byte into a list of 8 bytes
  def decompressBytes(byte: Byte): List[Byte] = {
    (0 to 7).map{ i => 
      if((byte & (1 << i)) != 0) 1.toByte
      else 0.toByte
    }.toList.reverse
  } 
  
  def apply(): Node = {
    val dOccs = inputOccs
    val dEdges = inputCompEdges
    val dDict = inputDict
    val decodedOccs = decodeOccs(dOccs, dDict)
    rebuiltTree(decodedOccs, dEdges)
  }

  def inputCompEdges: List[(Int, Int)] = {
    val size: Int = in.readInt
    val comp = (for (i <- 1 to size) yield ((in.readShort.toInt, in.readByte.toInt), in.readShort.toInt)).toList
    comp.map(e => (for(i <- (1 to e._2)) yield e._1).toList).flatten
  }

  def inputComp2Edges: List[(Int, Int)] = {
    val size1: Int = in.readInt
    val size2: Int = in.readInt
    val (p1, occ1) = (in.readShort.toInt, in.readShort.toInt)
    val inter = (for (i <- (1 to size1)) yield (in.readByte.toInt, in.readShort.toInt))
    val lp22 = (for (i <- (1 to size2)) yield (in.readShort.toInt, in.readShort.toInt))
    
    val lp11: List[(Int, Int)] = 
      (p1, occ1)::(inter.head._1 + p1, inter.head._2)::inter.tail.zip(inter).map(e => ((e._1._1 + p1 + e._2._1), e._1._2)).toList
    
    val lp1 = lp11.map(e => (for (i <- 1 to e._2) yield e._1).toList).flatten
    val lp2 = lp22.map(e => (for (i <- 1 to e._2) yield e._1).toList).flatten
    lp1.zip(lp2)
  }

}
