package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._
import scala.reflect.persistence.Enrichments._
import scala.reflect.persistence.Node._
import java.io.File
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.io.DataInputStream
import java.io.FileInputStream

class AstWriterTest extends FunSuite {

  test("WritingTest") {
    val treeStr = "c (m (v v (c (m v v) c (m v v))) m(v v (c c)) c (m (v v (c (m v v) c (m v v))) m(v v (c c)) ))"
    val tree = ParseTestTree.parse(treeStr).get

    val compressor = new AstCompressor()
    val decompressor = new AstDecompressor()
    val bytes: List[Byte] = compressor(tree)
    val recupTree = decompressor(bytes)
    assert(tree == recupTree, s"Error: Not matching:\n${tree}\nAnd\n${recupTree}")
  }
}
