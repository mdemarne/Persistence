package scala.reflect.persistence

import java.io._
import scala.language.existentials

/* Fetch trees and subtrees from the decompressed AST. */
/* For now : return the entire AST, take a universe as argument */
class ToolBox(val u: scala.reflect.api.Universe) {
  import u._
  import Enrichments._
  
  /* General function returning the whole tree */
  def getTst(source: String): Tree = {
    
    val src: java.io.DataInputStream = new DataInputStream(this.getClass().getResourceAsStream(source))
    
    //val nodeTree = new AstDecompressor(src)()

    /* TODO: rebuilt the tree from each part, ASTs, Symbols, etc. */
    //val tree = new TreeRecomposer[u.type](u)(nodeTree, ???, ???, ???, ???)
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

  def extractSubTBFS(nodes: List[NodeBFS]): List[NodeBFS] = {
    def loop(nds: List[NodeBFS], acc: List[NodeBFS]): List[NodeBFS] = nds match {
      case Nil => acc
      case n::ns if(acc.exists( e => e.bfsIdx == n.parentBfsIdx)) => 
        loop(acc:::List(n), ns)
      case n::ns => loop(acc, ns)
    }
    /*TODO doesn't work yet need to know where to start*/
    loop(Nil, nodes)
  }
  /*Need some way to know the index where looking for*/
  def extractSub(node: Node) : Node = {
    val bfs: List[NodeBFS] = node.flattenBFSIdx
    //TODO this is to be replaced
    val idx: Int = 34
    val(_, good) = bfs.splitAt(idx)
    val subT: List[NodeBFS] = extractSubTBFS(good) //TODO complete that
    subT.toTree
  } 
}
