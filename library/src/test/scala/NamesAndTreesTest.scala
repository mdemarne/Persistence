package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._

class NamesAndTreesTest extends FunSuite {

  def compressionTest(treeStr: String) {
    val (tree, names) = ParseTestTreeAndName.parse(treeStr).get
    /*println(s"The tree: ${tree}")
    println(s"The names: ${names}")
    println(s"The bfsTree: ${tree.flattenBFSIdx}")*/
    
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

  }

  test("First tree") {
    val treeStr = "c !coucou! (m !coucou! e !coucou! v !salut!)"
    compressionTest(treeStr)
  }
}
