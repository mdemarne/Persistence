package scala.reflect.persistence

import java.io._
import scala.language.existentials
/*TODO transform most of the functions into private ones*/
/* Fetch trees and subtrees from the decompressed AST. */
class ToolBox(val u: scala.reflect.api.Universe) {
  import u._
  import Enrichments._
  
  type NameDict = Map[String, List[Int]]
  type ConstantDict = Map[String, List[Int]]
  
  var saved: Map[String, (Node, RevList[NodeBFS], NameDict, ConstantDict)] = Map()

  def getSource(s: Symbol): Tree = {
    val fullName: List[String] = s.fullName.split(".").toList
    val path = fullName.init.mkString("/")
    val name = fullName.last

    def inner(ss: Symbol, file: String, n: String): Tree = ss.info match {
      case ModuleDef => getMethodDef(file, n)
      case ClassDef => getClassDef(file, n)
      case TypeDef => getTypeDef(file, n)
      case LabelDef => getLabelDef(file, n)
      case DefDef => getMethodDef(file, n)
      case ValDef => getValDef(file, n)
      case _ => throw new Exception(s"Error: Type of symbol not supported, ${n} in ${file}")
    }
    inner(s, path, name)
  }
  /* General function returning the whole tree */
  def getAst(file: String): Tree = {

    val src: java.io.DataInputStream = new DataInputStream(this.getClass().getResourceAsStream(file))
    val bytes: List[Byte] = new XZReader(src)()
    src.close()
    val (nodeTree, rest1) = new AstDecompressor()(bytes.tail)
    val (names, rest2) = new NameDecompressor()(rest1)
    val constants = new ConstantDecompressor()(rest2)

    /* TODO: rebuilt the tree from each part, ASTs, Symbols, etc. */
    new TreeRecomposer[u.type](u)(DecTree(nodeTree.flattenBFSIdx, names, constants))
  }

  def getMethodDef(file: String, name: String): Tree = getElement(file, name, NodeTag.DefDef)
  def getValDef(file: String, name: String): Tree = getElement(file, name, NodeTag.ValDef)
  def getModuleDef(file: String, name: String): Tree = getElement(file, name, NodeTag.ModuleDef)
  def getClassDef(file: String, name: String): Tree = getElement(file, name, NodeTag.ClassDef)
  def getTypeDef(file: String, name: String): Tree = getElement(file, name, NodeTag.TypeDef)
  def getLabelDef(file: String, name: String): Tree = getElement(file, name, NodeTag.LabelDef)

  def getNewElement(file: String, name: String, tpe: NodeTag.Value): Tree = {
    val (nodeTree, flatNames, flatConstants) = nameBasedRead(file, name)
    val bfs: RevList[NodeBFS] = nodeTree.flattenBFSIdx
    val names: Map[String, List[Int]] = initNames(flatNames, bfs)
    val constants = initConstants(flatConstants, bfs)
    saved += (file -> (nodeTree, bfs, names, constants))
    val index: Int = findIndex(bfs, tpe, names(name))
    if (index == -1)
      throw new Exception(s"Error: ${name} is not defined here")
    val subtree: List[NodeBFS] = extractSubBFS(bfs.reverse.drop(index))
    new TreeRecomposer[u.type](u)(DecTree(subtree, names, constants))
  }

  def getElement(file: String, name: String, tpe: NodeTag.Value): Tree = {
    if (!saved.contains(file)) {
      getNewElement(file, name, tpe)
    } else {
      val (_, bfs, names, constants) = saved(file)
      val index: Int = findIndex(bfs, tpe, names(name))
      if (index == -1)
        throw new Exception(s"Error: ${name} is not defined here")
      val subtree: List[NodeBFS] = extractSubBFS(bfs.reverse.drop(index))
      new TreeRecomposer[u.type](u)(DecTree(subtree, names, constants))

    }
  }
  /* Initialize the positions of the names in BFS order */
  def initNames(names: Map[String, List[Int]], nbfs: RevList[NodeBFS]): NameDict = {
    val toZip: List[(Int, String)] = names.map(x => x._2.map(y => (y, x._1))).toList.flatten.sortBy(_._1)
    val interm = nbfs.reverse.filter(x => NodeTag.hasAName(x.node.tpe))
    val zipped = interm.zip(toZip).map { x =>
      (x._1.bfsIdx, x._2._2)
    }
    zipped.groupBy(_._2).map(x => (x._1, x._2.map(y => y._1))).toMap
  }
  /* Initialize the positions of the constants in BFS order */
  def initConstants(constants: Map[String, List[Int]], nbfs: RevList[NodeBFS]): ConstantDict = {
    val toZip: List[(Int, String)] = constants.map(x => x._2.map(y => (y, x._1))).toList.flatten.sortBy(_._1)
    val interm = nbfs.reverse.filter(x => x.node.tpe == NodeTag.Literal)
    val zipped = interm.zip(toZip).map { x =>
      (x._1.bfsIdx, x._2._2)
    }
    zipped.groupBy(_._2).map(x => (x._1, x._2.map(y => y._1))).toMap
  }
  /* Find the bfs index of the element that corresponds to our search */
  def findIndex(nodes: RevList[NodeBFS], tpe: NodeTag.Value, occs: List[Int]): Int = occs match {
    case o :: os =>
      val node: NodeBFS = nodes.find(_.bfsIdx == occs.head).get
      if (node.node.tpe == tpe) {
        o
      } else
        findIndex(nodes, tpe, os)
    case _ =>
      -1
  }
  /* Helper function that reads all the elements we need to reconstruct the tree */
  def nameBasedRead(file: String, name: String): (Node, NameDict, ConstantDict) = {
    val src: java.io.DataInputStream = new DataInputStream(this.getClass().getResourceAsStream(file))
    val bytes: List[Byte] = new XZReader(src)()
    val (nodeTree, rest1) = new AstDecompressor()(bytes.tail)
    val (names, rest2) = new NameDecompressor()(rest1)
    val constants = new ConstantDecompressor()(rest2)
    if (!names.contains(name))
      throw new Exception("Error: specified name doesn't exist")
    src.close()
    (nodeTree, names, constants)
  }

  /* @warning must give the list of nodes in normal BFS and head is the node we need */
  def extractSubBFS(nodes: List[NodeBFS]): RevList[NodeBFS] = {
    assert(!nodes.isEmpty)
    def loop(nds: List[NodeBFS], acc: RevList[NodeBFS]): RevList[NodeBFS] = nds match {
      case Nil => acc
      case n :: ns if (acc.head.bfsIdx > n.bfsIdx && !acc.exists(_.bfsIdx == n.parentBfsIdx)) =>
        acc
      case n :: ns =>
        loop(ns, n :: acc)
    }
    loop(nodes.tail, nodes.head :: Nil)
  }
}
