import org.scalatest.FunSuite
import scala.reflect.persistence._

class AstCompressorTest extends FunSuite {

  test("parseTreeTest1") {

    val treeStr = "n ( n ( n n ) n )"

    val tree = ParseTestTree.parse(treeStr)
    val cmp = new AstCompressor(null)
    val dict = cmp.parse(tree.get)
    val exploitableDict = ParseTestTree.dictForTest(dict)
    println("Dictionary:")
    println(exploitableDict)

    assert(exploitableDict.size == 2)
    assert(exploitableDict.head._2 == 4)
    assert(exploitableDict.tail.head._2 == 1)
  }

}
