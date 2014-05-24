package scala.reflect.persistence

import java.io.DataInputStream
import org.tukaani.xz.SingleXZInputStream
/* TODO: make some functions private. Here public for tests */
class AstDecompressor(in: DataInputStream) {
  import Enrichments._
  
  var toRead: List[Byte] = Nil

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
  def inputOccs: List[Byte] = readBytes(readShort)
  def inputDict: RevHufDict = {
    var dict: RevHufDict = Map()
    val nbEntries = readInt
    (0 until nbEntries).foreach{ i => 
      val sizeHuff: Int = readInt
      val huffcode = readBytes(sizeHuff)
      val nbNode: Int = readShort
      val ndBfs: List[NodeBFS] = 
        (for(i <- 1 to nbNode) 
          yield (NodeTag.getVal(readByte.toInt), readShort.toInt, readShort.toInt)).toList.map(i => NodeBFS(Node(i._1, Nil), i._2, i._3))
      dict += (huffcode -> ndBfs)
    }
    dict
  }
  def readBytes(size: Int): List[Byte] = {
    var bytes: List[Byte] = Nil
    for(i <- (0 until Math.ceil(size.toDouble / 8).toInt)){
      //bytes  :+= in.readByte
      bytes :+= readByte
    }
    bytes.map(decompressBytes(_)).reverse.flatten.drop((8 - (size % 8)) % 8)
  }
  def inputEdges: List[(Int, Int)] = {
    val size: Int = in.readInt
    (for(i <- 1 to size) yield (in.readShort.toInt, in.readShort.toInt)).toList
  }
  /* Decompresses the byte into a list of 8 bytes */
  def decompressBytes(byte: Byte): List[Byte] = {
    (0 to 7).map{ i => 
      if((byte & (1 << i)) != 0) 1.toByte
      else 0.toByte
    }.toList.reverse
  } 
  def apply(): Node = {
    toRead = Nil
    unapplyXZ
    val hasNames = readByte
    //TODO modify this
    if(hasNames == 1)
      inputNames
    val dOccs = inputOccs
    val dEdges = inputComp2Edges
    val dDict = inputDict
    val decodedOccs = decodeOccs(dOccs, dDict)
    rebuiltTree(decodedOccs, dEdges)
  }
  def inputCompEdges: List[(Int, Int)] = {
    val size: Int = in.readInt
    var sum: Int = 0
    var comp: List[((Int, Int), Int)] = Nil
    do {
      comp :+= ((in.readShort.toInt, in.readShort.toInt), in.readShort.toInt)
      sum += comp.last._2
    } while (sum < size)

    comp.map(e => (for(i <- (1 to e._2)) yield e._1).toList).flatten
  }
  def inputComp2Edges: List[(Int, Int)] = {
   val size: Int = readInt
   //Read the first list
   var l1: List[(Int, Int)] = (readShort.toInt, readShort.toInt)::Nil
   var sum: Int = l1.head._2
   while(sum < size) {
    l1 :+= (readByte.toInt, readShort.toInt)
    sum += l1.last._2
   } 
  /* Decompress the first list */
   val l1f: List[Int] = l1.zipWithIndex.map{ e => 
    val value = l1.zipWithIndex.filter(x => x._2 <= e._2).map(_._1._1).sum
    (for(i <- 1 to e._1._2) yield (value)).toList
   }.flatten
   //Read the second list
   var l2: List[(Int, Int)] = (readShort.toInt, readShort.toInt)::Nil
   sum = l2.head._2
   while(sum < size ) {
    l2 :+= (readShort.toInt, readShort.toInt)
    sum += l2.last._2
   } 
  
  /* Decompress the second list */
   val l2f: List[Int] = l2.map{e => (for(i <- 1 to e._2) yield e._1).toList}.flatten
   assert(l1f.size == l2f.size, s"l1f has ${l1f.size} l2f has ${l2f.size}")
   l1f.zip(l2f)
  }


  def readInt: Int = toRead match {
    case w::x::y::z::xs => 
      toRead = xs
      ((w) + (x << 8) + (y << 16) + (z << 24)).toInt
    case x => 
      throw new Exception("Error: Decompressor cannot read an Int from ${x}")
  }

  def readShort: Short = toRead match {
    case x::y::xs => 
      toRead = xs
      ((x) + (y << 8)).toShort
    case x =>
      throw new Exception(s"Error: Decompressor cannot read Short from ${x}")
  }

  def readByte: Byte = toRead match {
    case x::xs => 
      toRead = xs
      x
    case x => 
      throw new Exception("Error: Decompressor cannot read Byte ${x}")
  }

  def unapplyXZ {
    val total: Long = in.readLong
    val decomp: SingleXZInputStream = new SingleXZInputStream(in)
    for(i <- (1.toLong to total)){
      toRead :+= (decomp.read()).toByte
    }
  }

  def inputNames: Map[String, (List[Int], Boolean)] = {
    val size = readInt
    var res: Map[String, (List[Int], Boolean)] = Map()
    (0 until size).foreach{ y => 
      val isTerm = readByte
      val isTermName: Boolean = if (isTerm == 0) false else true
      val name: String = toRead.takeWhile(_ != '\n'.toByte).mkString
      toRead = toRead.dropWhile(_ != '\n').tail
      val entriesSize = readShort
      var entries: List[Int] = (0 until entriesSize).map( x => readShort.toInt).toList
      res += (name -> (entries, isTermName))
    }
    res
  }
}
