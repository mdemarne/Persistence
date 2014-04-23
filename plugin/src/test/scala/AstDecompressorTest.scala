import org.scalatest.FunSuite
import scala.reflect.persistence._
import scala.reflect.persistence.Enrichments._

class AstDecompressorTest extends FunSuite {

  /* NB: assumes that the splitTree, genHuffman, encodeOccs are working */
  val (hufOccs, originOccs, dict, edges) = {
    val treeStr = "c (m (v v (c (m v v) c (m v v))) m(v v (c c)) c (m (v v (c (m v v) c (m v v))) m(v v (c c)) ))"
    val compressor = new AstCompressor(null)

    val tree = ParseTestTree.parse(treeStr)
    val splitTree = compressor.splitTree(tree.get)
    val hufCodes = compressor.genHuffman(splitTree._1)
    val occs = compressor.encodeOccs(splitTree._2, hufCodes)
    (occs, splitTree._2, hufCodes, splitTree._3)
  }

  test("Decoding occurences using reversed HufDict") {
    val decompressor = new AstDecompressor(null)
    val originOccsRec = decompressor.decodeOccs(hufOccs, dict)
    assert(originOccs == originOccsRec, "The decompressed subtrees should be equivalent.")
  }
}