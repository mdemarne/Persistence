package scala.reflect.persistence

/* Fetch trees and subtrees from the decompressed AST. */
/* For now : return the entire AST, take a universe as argument */
class ToolBox(val u: scala.reflect.api.Universe) {
  import u._
  import Enrichments._

  /* Wrapper for treeDecomposer's function */
  case class DecomposedTree(tree: Node, namesBFS: Map[Name, List[Int]], symbBFS: Map[Symbol, List[Int]], typesBFS: Map[Type, List[Int]], constBFS: Map[Constant, List[Int]])

  /* General function returning the whole tree */
  def getTst(source: String): Tree = {

    /* TODO: find the good source file */
    val source = ???
    val NodeTree = new AstDecompressor(source)()
    /* TODO: rebuilt the tree from each part, ASTs, Symbols, etc. */
    val decomposedTree: DecomposedTree = ???
    /* TODO: find a way to deal with the Universe. One-way import relation are OK, but here ToolBox passes an argument fo TreeRecomposer and return a value returned from TreeRecomposer. They both depend on the import of the other one... */
    /* new TreeRecomposer(u)(decomposedTree) */
    ???
  }
}