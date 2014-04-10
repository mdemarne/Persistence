import org.scalatest.FunSuite
import scala.reflect.persistence._

class AstCompressorTest extends FunSuite {

  test("parseTreeTest1") {

    val treeStr = "n ( n ( n n ) n )"

    val tree = ParseTestTree.parse(treeStr)
    val cmp = new AstCompressor(null)
    val dict = tree.get.computeFreqs
    val exploitableDict = ParseTestTree.dictForTest(dict)
    println("Dictionary:")
    println(exploitableDict)

    assert(exploitableDict.size == 2)
    assert(exploitableDict.head._2 == 4)
    assert(exploitableDict.tail.head._2 == 1)
  }

  test("parseTreeTest2") {

    val treeStr = "c (n (m m v) m ( v v v v v ) m ( v v ) m (c v))"

    val tree = ParseTestTree.parse(treeStr)
    val cmp = new AstCompressor(null)
    val dict = tree.get.computeFreqs
    val exploitableDict = ParseTestTree.dictForTest(dict)
    println("Dictionary:")
    println(exploitableDict)
    assert(exploitableDict.size == 7)
  }

  test("parseTreeTest3") {
    val treeStr = "c (v v c (v v) c(v v) c(v v))"

    val tree = ParseTestTree.parse(treeStr)
    val cmp = new AstCompressor(null)
    val dict = tree.get.computeFreqs
    val exploitableDict = ParseTestTree.dictForTest(dict)
    println("Dictionary:")
    println(exploitableDict)
    assert(exploitableDict.size == 4)
    val entry = ParseTestTree.dictEntry(AstTag.ValDef, 0, -1)
    assert(exploitableDict.contains(List(entry)))
    assert(exploitableDict(List(entry)) == 1)
    assert(exploitableDict.keys.exists(_.size == 3))
    assert(exploitableDict(exploitableDict.keys.find(_.size == 3).get)== 3)
  }

  test("DoublePattern") {
    val treeStr = "c (m (v v (c (m v v) c (m (v v)))) m(v v (c c)))"
    
    val cmp = new AstCompressor(null)
    val tree = ParseTestTree.parse(treeStr)
    val dict = ParseTestTree.dictForTest(tree.get.computeFreqs)
    println("Dictionary:")
    println(dict)
    assert(dict.values.toList.contains(7))
    assert(dict.values.toList.count(_ == 2) == 4)
    assert(dict.values.toList.count(_ == 1) == 2)
  }

}
