package scala.reflect.persistence

import java.io._
import scala.language.existentials

/* Fetch trees and subtrees from the decompressed AST. */
/* For now : return the entire AST, take a universe as argument */
class ToolBox(val u: scala.reflect.api.Universe) {
  import u._
  import Enrichments._
  
  /* General function returning the whole tree */
  /* TODO: figure out how we want to deal with the source */
  def getTst(source: String): Tree = {
    
    //TODO I think jar containing AST needs to be in classpath (lib)
    //TODO throws NullPointerException if does not exists
    val src: java.io.DataInputStream = new DataInputStream(this.getClass().getResourceAsStream(source))
    
    val nodeTree = new AstDecompressor(src)()

    /* TODO: rebuilt the tree from each part, ASTs, Symbols, etc. */
    val tree = new TreeRecomposer[u.type](u)(nodeTree, ???, ???, ???, ???)
    ???
  }

  def getMethodDef(file: String, name: String): Tree = {
    ???
  }

  def getValDef(file: String, name: String): Tree = {
    ???
  }

  def getClass(file: String, name: String): Tree = {
    ???
  }
}
