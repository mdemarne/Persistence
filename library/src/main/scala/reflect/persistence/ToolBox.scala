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
    val bytes: List[Byte] = new XZReader(src)()
    val hasNames: Boolean = (bytes.head == 1.toByte)
    val decompressor: AstDecompressor = new AstDecompressor()
    val nodeTree = decompressor(bytes.tail)
    val toRead: List[Byte] = decompressor.getToRead 
    var names: Map[String, List[Int]] = Map()
    if(hasNames)
      names = (new NameDecompressor()(toRead))._1
    
    /* TODO: rebuilt the tree from each part, ASTs, Symbols, etc. */
    val tree = new TreeRecomposer[u.type](u)(nodeTree, ???, ???, ???, ???)
    ???
  }

  def getMethodDef(file: String, name: String): Tree = {
    val (nodeTree, names) = nameBasedRead(file, name)
    val bfs: List[NodeBFS] = nodeTree.flattenBFSIdx
    val index: Int = findIndex(bfs, NodeTag.DefDef, names(name))
    ???
  }

  def getValDef(file: String, name: String): Tree = {
    val (nodeTree, names) = nameBasedRead(file, name)
    val bfs: List[NodeBFS] = nodeTree.flattenBFSIdx
    val index: Int = findIndex(bfs, NodeTag.ValDef, names(name))
   ???
  }
  
  def getObject(file: String, name: String): Tree = {
    val (nodeTree, names) = nameBasedRead(file, name)
    val bfs: List[NodeBFS] = nodeTree.flattenBFSIdx
    val index: Int = findIndex(bfs, NodeTag.ModuleDef, names(name))
    ???
  }

  def getClassDef(file: String, name: String): Tree = {
    val (nodeTree, names) = nameBasedRead(file, name)
    val bfs: List[NodeBFS] = nodeTree.flattenBFSIdx
    val index: Int = findIndex(bfs, NodeTag.ClassDef, names(name))
    ???
  }
 
  def getTypeDef(file: String, name: String): Tree = {
    val (nodeTree, names) = nameBasedRead(file, name)
    val bfs: List[NodeBFS] = nodeTree.flattenBFSIdx 
    val index: Int = findIndex(bfs, NodeTag.TypeDef, names(name))
    ???
  }

  def getLabelDef(file: String, name: String): Tree = {
    val (nodeTree, names) = nameBasedRead(file, name)
    val bfs: List[NodeBFS] = nodeTree.flattenBFSIdx 
    val index: Int = findIndex(bfs, NodeTag.LabelDef, names(name))
    ???
  }

  /*Find the bfs index of the element that corresponds to our search*/
  private def findIndex(nodes: RevList[NodeBFS], tpe: NodeTag.Value, occs: List[Int]): Int = occs match{
    case o::os =>
      /*TODO check if not possible to get the element at index o instead*/
      val node: NodeBFS = nodes.find(_.bfsIdx == occs.head).get
      if(node.node.tpe == tpe)
        o
      else if (! NodeTag.isADefine(node.node.tpe))
        -1
      else 
        findIndex(nodes, tpe, os)
    case _ => 
      -1
  }
  /*Helper function that reads all the elements we need to reconstruct the tree*/
  private def nameBasedRead(file: String, name: String): (Node, Map[String, List[Int]]) = {
    val src: java.io.DataInputStream = new DataInputStream(this.getClass().getResourceAsStream(file))
    val bytes: List[Byte] = new XZReader(src)()
    val hasNames: Boolean = (bytes.head == 1.toByte)
    if(!hasNames)
      throw new Exception("Error: names are not saved !")
    val decompressor: AstDecompressor = new AstDecompressor()
    val nodeTree: Node = decompressor(bytes.tail)
    val toRead: List[Byte] = decompressor.getToRead
    var names: Map[String, List[Int]] = (new NameDecompressor()(toRead))._1
    if(!names.contains(name))
      throw new Exception("Error: specified name doesn't exist")
    src.close()
    (nodeTree, names)
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
