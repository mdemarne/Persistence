package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._

class NamesAndTreesTest extends FunSuite {
  import Enrichments._
  def compressionTest(treeStr: String, name: String, tpe: NodeTag.Value, expected: String) {
    def bringBackTheFunk(m: Map[String, List[Int]]): Map[String, List[Int]] = {
      m.map(x => (x._1, x._2.sorted))
    }
    val (tree, names) = ParseTestTreeAndName.parse(treeStr).get
    val tool: ToolBox = new ToolBox (scala.reflect.runtime.universe)
    
    /*Testing Ast compression*/
    val compressor = new AstCompressor()
    val decompressor = new AstDecompressor()
    val bytes: List[Byte] = compressor(tree)
    val recupTree = decompressor(bytes)._1

    assert(tree == recupTree, s"${tree}\n did not match\n${recupTree}")
    
    /*Testing the names*/
    val nameComp: NameCompressor = new NameCompressor()
    val nameDecomp: NameDecompressor = new NameDecompressor()

    val nameBytes: List[Byte] = nameComp(names)

    val recupNames: Map[String, List[Int]] = tool.initNames(nameDecomp(nameBytes)._1, recupTree.flattenBFSIdx)
    assert(bringBackTheFunk(recupNames) == bringBackTheFunk(names), s"\n${names}\n did not match\n${recupNames}")
    

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

  test("Second tree: complicated with different names") {
    val treeStr = "c !first! (m !child! (v v (c (m !Shabat! v v) c (m (v !vindalo! v)))) m(v v (c c !last!)))"
    compressionTest(treeStr, "first", NodeTag.ClassDef, "c (m (v v (c (m v v) c (m (v v)))) m(v v (c c)))")
    compressionTest(treeStr, "Shabat", NodeTag.ModuleDef, "m")
    compressionTest(treeStr, "child", NodeTag.ModuleDef, "m (v v (c (m v v) c (m (v v))))")
    compressionTest(treeStr, "vindalo", NodeTag.ValDef, "v")
    compressionTest(treeStr, "last", NodeTag.ClassDef, "c")
  }

  test("Third tree: Multiple levels same name different types") {
    val treeStr = "c !coucou! (m (v (m m) v !hello! (c !hello!)) v !coucou! (m (c c !youpla!)))"

    compressionTest(treeStr, "coucou", NodeTag.ValDef, "v (m (c c))")
    compressionTest(treeStr, "hello", NodeTag.ValDef, "v (c)")
    compressionTest(treeStr, "hello", NodeTag.ClassDef, "c")
    compressionTest(treeStr, "coucou", NodeTag.ClassDef, "c (m (v (m m) v (c)) v (m (c c)))")
    compressionTest(treeStr, "youpla", NodeTag.ClassDef, "c")
  
  }

}
