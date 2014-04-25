import org.scalatest.FunSuite
import scala.reflect.persistence._
import scala.reflect.persistence.Enrichments._

class AstDecompressorTest extends FunSuite {

  /* NB: assumes that the splitTree, genHuffman, encodeOccs are working */
  val (tree,hufOccs, originOccs, dict, edges) = {
    val treeStr = "c (n (m m v) )"
    val compressor = new AstCompressor(null)

    val tree = ParseTestTree.parse(treeStr)
    val splitTree = compressor.splitTree(tree.get)
    val hufCodes = compressor.genHuffman(splitTree._1)
    val occs = compressor.encodeOccs(splitTree._2, hufCodes)
    (tree, occs, splitTree._2, hufCodes, splitTree._3)
  }
  
  test("Just print the dictionary") {
    println("dictionary.")
    println(dict)
  }

  test("Decoding occurrences using reversed HufDict") {
    val decompressor = new AstDecompressor(null)
    println("origin occurrences:")
    println(originOccs)
    val originOccsRec = decompressor.decodeOccs(hufOccs, dict.map(_.swap))
    println("Decompressed Occs:")
    println(originOccsRec)
    assert(originOccs == originOccsRec, "The decompressed subtrees should be equivalent to the original.")
  }
  
  test("Rebuilding a tree once compressed") {
    val decompressor = new AstDecompressor(null)
    val originOccsRec = decompressor.decodeOccs(hufOccs, dict.map(_.swap))
    println("Orignal Tree:")
    println(tree.get)
    val treeRec = decompressor.rebuiltTree(originOccsRec, edges.tail)
    println("Recomposed Tree:")
    println(treeRec)
    assert(tree.get == treeRec, "The decompressed tree do not match the oroginal one.")
  }
}