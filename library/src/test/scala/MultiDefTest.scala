package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._

class MultiDefTest extends FunSuite {
  import Enrichments._
  
  def compressionTest(treeStr: String, fullName: List[String], expected: String) {
    val (tree, names) = ParseTestTreeAndName.parse(treeStr).get
    val tool: ToolBox = new ToolBox(scala.reflect.runtime.universe)

    /*We compress the tree*/
    val compressor = new AstCompressor()
    val decompressor = new AstDecompressor()
    val bytes: List[Byte] = compressor(tree)
    val recupTree = decompressor(bytes)

    assert(tree == recupTree)

    /*Check the extraction of subtrees*/
    val bfs: RevList[NodeBFS] = recupTree.flattenBFSIdx
    val subtree: Node = tool.findWithFullPath(fullName, names, bfs.reverse).toTree
    val correction: Node = ParseTestTree.parse(expected).get
    assert(subtree == correction)
  }


  test("First Tree: Simple multi definition") {
    val str: String = "c !one! (m !two! ( v !three!) m !four! (v !three! ( m !five!)))"
    val fullName1: List[String] = List("one", "two", "three")
    val fullName2: List[String] = List("one", "four", "three")
    compressionTest(str, fullName1, "v")
    compressionTest(str, fullName2, "v ( m )")
  }

  test("Second Tree: Deep definition") {
    val str: String = "c !o! ( v !t! ( c !t! m !u! (v !t!)) v !t! ( m !o!))"
    val fullName1: List[String] = List("o", "t", "t")
    val fullName2: List[String] = List("o", "t", "o")
    val fullName3: List[String] = List("o", "t", "u", "t")
    compressionTest(str, fullName1, "c")
    compressionTest(str, fullName2, "m")
    compressionTest(str, fullName3, "v")
  
  }
  
}
