package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._

class MultiDefTest extends FunSuite {
  import Enrichments._

  def compressionTest(treeStr: String, fullName: List[String], expected: String, tpe: NodeTag.Value) {
    val (tree, names) = ParseTestTreeAndName.parse(treeStr).get
    val tool: ToolBox = new ToolBox(scala.reflect.runtime.universe)

    /*We compress the tree*/
    val compressor = new AstCompressor()
    val decompressor = new AstDecompressor()
    val bytes: List[Byte] = compressor(tree)
    val recupTree = decompressor(bytes)._1

    assert(tree == recupTree)

    /*Check the extraction of subtrees*/
    val bfs: RevList[NodeBFS] = recupTree.flattenBFSIdx
    val subtree: Node = tool.findDefinition(fullName, names, bfs.reverse, tpe).toTree
    val correction: Node = ParseTestTree.parse(expected).get
    assert(subtree == correction)
  }

  test("First Tree: Simple multi definition") {
    val str: String = "c !one! (m !two! ( v !three!) m !four! (v !three! ( m !five!)))"
    val fullName1: List[String] = List("one", "two", "three")
    val fullName2: List[String] = List("one", "four", "three")
    compressionTest(str, fullName1, "v", NodeTag.ValDef)
    compressionTest(str, fullName2, "v ( m )", NodeTag.ValDef)
  }

  test("Second Tree: Deep definition") {
    val str: String = "c !o! ( v !t! ( c !t! m !u! (v !t!)) v !t! ( m !o!))"
    val fullName1: List[String] = List("o", "t", "t")
    val fullName2: List[String] = List("o", "t", "o")
    val fullName3: List[String] = List("o", "t", "u", "t")
    compressionTest(str, fullName1, "c", NodeTag.ClassDef)
    compressionTest(str, fullName2, "m", NodeTag.ModuleDef)
    compressionTest(str, fullName3, "v", NodeTag.ValDef)

  }

  test("Third Tree: Same path name, different types") {
    val str: String = "c !o! (m !v! (v !m!) m !v! (c !m!) m !v! (m !m!))"
    val fullName: List[String] = List("o", "v", "m")
    compressionTest(str, fullName, "v", NodeTag.ValDef)
    compressionTest(str, fullName, "c", NodeTag.ClassDef)
    compressionTest(str, fullName, "m", NodeTag.ModuleDef)
  }

}
