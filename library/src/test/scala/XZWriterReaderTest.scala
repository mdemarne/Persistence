package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._
import scala.annotation.tailrec
import scala.reflect.persistence.Enrichments._
import java.io.File
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.io.DataInputStream
import java.io.FileInputStream
import scala.language.existentials

class XZWriterReaderTest extends FunSuite {
  val u = scala.reflect.runtime.universe
  import u._

  test("Simply write and read some bytes") {
    val testFile = new File("hello.ast")
    val writer = new XZWriter(new DataOutputStream(new FileOutputStream(testFile)))
    val reader = new XZReader(new DataInputStream(new FileInputStream(testFile)))

    val str = ("Hello world\n\t".getBytes.toList :+ -1.toByte) ::: "next".getBytes.toList
    writer(str)
    val rr = reader()
    assert(str == rr, "Writing to file does not work.")
    /* println(rr.map(_.toChar).mkString) */
    testFile.delete
  }
  test("test compressing on real trees") {

    val testFile = new File("hello.ast")
    val tree = reify { def test = "simple test" }.tree

    /* Let's just put all of that into a file as if it was the plugin executing */
    val decTree = new TreeDecomposer[u.type](u)(tree)
    val compAsts = new AstCompressor()(decTree.treeBFS.toTree)
    val compNames = new NameCompressor()(decTree.names)
    val compConstants = new ConstantCompressor[u.type](u)(decTree.constants.map(v => (v._1, v._2)))
    new XZWriter(new DataOutputStream(new FileOutputStream(testFile.toString)))(compAsts ++ compNames ++ compConstants)

    /* Now let's read it */
    val src: java.io.DataInputStream = new DataInputStream(new FileInputStream(testFile))
    val bytes: List[Byte] = new XZReader(src)()
    src.close()

    val (nodeTree, rest1) = new AstDecompressor()(bytes)
    val (names, rest2) = new NameDecompressor()(rest1)
    val constants = new ConstantDecompressor[u.type](u)(rest2)

    /* Check if what is read corresponds to what is written */
    assert((compAsts ++ compNames ++ compConstants) == bytes, "Compressed/decompressed list of bytes does not match")
    /* Check if the nodeTree decompressed corresponds to the ones compressed */
    assert(decTree.treeBFS == nodeTree.flattenBFSIdx, "Compressed/decompressed NodesBFS does not match")
    /* Check if the decompressed names corresponds to the ones compressed */
    assert(decTree.names == names, "Compressed/decompressed Names does not match")
    /* Check if the decompressed constants match the ones compressed */
    assert(decTree.constants == constants, "Compressed/decompressed constants do not match")

    val toolbox = new ToolBox(u)
    val flatNode = nodeTree.flattenBFSIdx
    val treeDec = new TreeRecomposer[u.type](u)(DecTree(flatNode, toolbox.initNames(names, flatNode), toolbox.initConstants(constants, flatNode)))

    /* Check that the two tree do match */
    assert(showRaw(tree) == showRaw(treeDec), "The two tree should match")
  }
}