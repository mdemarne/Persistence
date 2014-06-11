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
    println(s"My tree mother fucka: ${subtree}")
    val correction: Node = ParseTestTree.parse(expected).get
    assert(subtree == correction)
  }


  test("First Tree: Simple multi definition") {
    val str: String = "c !one! (m !two! ( v !three!) m !four! (v !three! ( m )))"
    val fullName1: List[String] = List("one", "two", "three")
    val fullName2: List[String] = List("one", "four", "three")
    compressionTest(str, fullName1, "v")
    //compressionTest(str, fullName2, "v ( m )")
  }
  
}
