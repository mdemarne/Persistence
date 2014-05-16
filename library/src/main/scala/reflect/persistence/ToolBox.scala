package scala.reflect.persistence

import java.io.DataInputStream
import scala.language.existentials

/* Fetch trees and subtrees from the decompressed AST. */
/* For now : return the entire AST, take a universe as argument */
class ToolBox(val u: scala.reflect.api.Universe) {
  import u._
  import Enrichments._

  /* General function returning the whole tree */
  /* TODO: figure out how we want to deal with the source */
  def getTst(source: String): Tree = {

    val src: java.io.DataInputStream = ???
    val nodeTree = new AstDecompressor(src)()

    /* TODO: rebuilt the tree from each part, ASTs, Symbols, etc. */
    val tree = new TreeRecomposer[u.type](u)(nodeTree, ???, ???, ???, ???)
    ???
  }
}