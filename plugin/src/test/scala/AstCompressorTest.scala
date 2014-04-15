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

  /* Tests concerning the tree actually used for compression */

  test("DictTreeTest1") {
    
    val treeStr = "c (m (v v (c (m v v) c (m (v v)))) m(v v (c c)))"

    val tree = ParseTestTree.parse(treeStr)
    val compressionDict = new AstCompressor(null).splitTree(tree.get)._1
    println("Used Dictionary:")
    compressionDict.testingDict foreach(println(_))
  }

}
