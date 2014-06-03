package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._

class NamesAndTreesTest extends FunSuite {
  import Enrichments._
  def compressionTest(treeStr: String, name: String, tpe: NodeTag.Value, expected: String) {
    val (tree, names) = ParseTestTreeAndName.parse(treeStr).get
    val tool: ToolBox = new ToolBox (scala.reflect.runtime.universe)
    
    /*Testing Ast compression*/
    val compressor = new AstCompressor()
    val decompressor = new AstDecompressor()
    val bytes: List[Byte] = compressor(tree)
    val recupTree = decompressor(bytes)

    assert(tree == recupTree, s"${tree}\n did not match\n${recupTree}")
    
    /*Testing the names*/
    val nameComp: NameCompressor = new NameCompressor()
    val nameDecomp: NameDecompressor = new NameDecompressor()

    val nameBytes: List[Byte] = nameComp(names)
    val recupNames: Map[String, List[Int]] = nameDecomp(nameBytes)._1
    assert(recupNames == names, s"\n${names}\n did not match\n${recupNames}")

    /*Testing the extraction of one part*/
    val bfs: RevList[NodeBFS] = recupTree.flattenBFSIdx
    val index: Int = tool.findIndex(bfs, tpe, names(name))
    val subtree: Node = tool.extractSubBFS(bfs.reverse.drop(index)).toTree
    val correction: Node = ParseTestTree.parse(expected).get
    assert(subtree == correction, s"Extract ${name}")


  }

  test("First tree: Basic") {
    val treeStr = "c !coucou! (m !coucou! v !yo! v !salut!)"
    compressionTest(treeStr, "coucou", NodeTag.ClassDef, "c (m v v)")
    compressionTest(treeStr, "coucou", NodeTag.ModuleDef, "m")
    compressionTest(treeStr, "yo", NodeTag.ValDef, "v")
    compressionTest(treeStr, "salut", NodeTag.ValDef, "v")
  }

  test("Second Tree: complicated with different names") {
    val treeStr = "c !first! (m !child! (v v (c (m !Shabat! v v) c (m (v !vindalo! v)))) m(v v (c c !last!)))"
    compressionTest(treeStr, "first", NodeTag.ClassDef, "c (m (v v (c (m v v) c (m (v v)))) m(v v (c c)))")
    compressionTest(treeStr, "Shabat", NodeTag.ModuleDef, "m")
    //TODO problem
    compressionTest(treeStr, "child", NodeTag.ModuleDef, "m (v v (c (m v v) c (m (v v))))")
    compressionTest(treeStr, "vindalo", NodeTag.ValDef, "v")
    compressionTest(treeStr, "last", NodeTag.ClassDef, "c")
  }
}
