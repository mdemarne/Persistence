package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._
import java.io.File
import java.io.DataOutputStream
import java.io.DataInputStream
import java.io.FileOutputStream
import java.io.FileInputStream

class ASTpersistenceTest extends FunSuite {
  var count = 0
  def compWriteReadDecomp(treeStr: String) {
    val tree = ParseTestTree.parse(treeStr).get

    count += 1
    val file = new File("WriteTest"+count)
    val compressor = new AstCompressor(new DataOutputStream(new FileOutputStream(file)))
    val decompressor = new AstDecompressor(new DataInputStream(new FileInputStream(file)))

    compressor(tree)
    val recupTree = decompressor()

    assert(tree == recupTree)
  } 

  test("First Tree"){
    val treeStr = "c (m (v v (c (m v v) c (m v v))) m(v v (c c)) c (m (v v (c (m v v) c (m v v))) m(v v (c c)) ))"

    compWriteReadDecomp(treeStr)

  }
}
