import org.scalatest.FunSuite
import scala.reflect.persistence._
import scala.reflect.persistence.Enrichments._

class AstCompressorTest extends FunSuite {

  /* Tests concerning computeFreqs */

  test("parseTreeTest1") {

    val treeStr = "n ( n ( n n ) n )"

    val tree = ParseTestTree.parse(treeStr)
    val exploitableDict = tree.get.computeFreqs.testingDict
    println("Dictionary:")
    println(exploitableDict)

    assert(exploitableDict.size == 2)
    assert(exploitableDict.head._2 == 5)
    assert(exploitableDict.tail.head._2 == 1)
  }

  test("parseTreeTest2") {

    val treeStr = "c (n (m m v) m ( v v v v v ) m ( v v ) m (c v))"

    val tree = ParseTestTree.parse(treeStr)
    val exploitableDict = tree.get.computeFreqs.testingDict
    println("Dictionary:")
    println(exploitableDict)
    assert(exploitableDict.size == 8)
  }

  test("parseTreeTest3") {
    val treeStr = "c (v v c (v v) c(v v) c(v v))"

    val tree = ParseTestTree.parse(treeStr)
    val exploitableDict = tree.get.computeFreqs.testingDict
    println("Dictionary:")
    println(exploitableDict)
    assert(exploitableDict.size == 4)
    val entry = MetaEntry(NodeTag.ValDef, 0, -1)
    assert(exploitableDict.contains(List(entry)))
    assert(exploitableDict(List(entry)) == 3)
    assert(exploitableDict.keys.exists(_.size == 3))
    assert(exploitableDict(exploitableDict.keys.find(_.size == 3).get) == 3)
  }

  test("DoublePattern") {
    val treeStr = "c (m (v v (c (m v v) c (m (v v)))) m(v v (c c)))"

    val tree = ParseTestTree.parse(treeStr)
    val dict = tree.get.computeFreqs.testingDict
    println("Dictionary:")
    println(dict)
    assert(dict.values.toList.count(_ == 2) == 2)
    assert(dict.values.toList.count(_ == 1) == 3)
  }

  /* Tests concerning the dictionary actually used for compression */

  test("DictTreeTest1") {

    val treeStr = "c (m (v v (c (m v v) c (m v v))) m(v v (c c)) c (m (v v (c (m v v) c (m v v))) m(v v (c c)) ))"

    val tree = ParseTestTree.parse(treeStr)
    val splitTree = new AstCompressor(null).splitTree(tree.get)
    println("Used Dictionary:")
    splitTree._1.testingDict foreach (println(_))
    println("Edges:")
    println(splitTree._3)
    assert(splitTree._3.head._1 == -1, "The edge reference of the first encoded subtree should always be -1.")
    val nbOcc = splitTree._1.values.sum
    assert(splitTree._2.size == nbOcc, "the number of occurences should be the same as the sum of the frequencies of the dicitonary.")
  }
  

  test("HuffmanGenerationTest1") {
    
    val treeStr = "c (m (v v (c (m v v) c (m v v))) m(v v (c c)) c (m (v v (c (m v v) c (m v v))) m(v v (c c)) ))"
      val compressor = new AstCompressor(null) 

    val tree = ParseTestTree.parse(treeStr)
    val splitTree = compressor.splitTree(tree.get)
    val hufCodes = compressor.genHuffman(splitTree._1)
    println("Huffman codes used for compression:")
    hufCodes.values foreach (c => println(c.map(v => v.toInt)))
    println("Bit string for the compressed tree:")
    compressor.encodeOccurrences(splitTree._2, hufCodes) foreach(print(_))
    println()
    assert(splitTree._1.size == hufCodes.size, "Wrong size of Huffman codes !")
    
  }

}
