package scala.reflect.persistence

import java.io._
import scala.language.existentials
/*TODO transform most of the functions into private ones*/
/* Fetch trees and subtrees from the decompressed AST. */
class ToolBox(val u: scala.reflect.api.Universe) {
  import u._
  import Enrichments._

  type NameDict = Map[String, List[Int]]
  type ConstantDict = Map[Any, List[Int]]

  var saved: Map[String, (Node, RevList[NodeBFS], NameDict, ConstantDict)] = Map()

  /*TODO this version doesn't support fullName specification yet*/
  def getSource(s: Symbol): Tree = {
    val fullPath: List[String] = s.fullName.split(".").toList
    /*TODO check that this is correct*/
    val path = s.pos.source.file.path
    val name = fullPath.last
    /*Fully specified name for what we're looking for*/
    val fullName: List[String] = fullPath.drop(path.split("/").size).toList
    assert(fullName.last == name)
    def inner(ss: Symbol, file: String): Tree = ss.info match {
      case ModuleDef => getMethodDef(file, fullName)
      case ClassDef => getClassDef(file, fullName)
      case TypeDef => getTypeDef(file, fullName)
      case LabelDef => getLabelDef(file, fullName)
      case DefDef => getMethodDef(file, fullName)
      case ValDef => getValDef(file, fullName)
      case _ => throw new Exception(s"Error: Type of symbol not supported, ${fullName.last} in ${file}")
    }
    inner(s, path)
  }
  /* General function returning the whole tree */
  def getAst(file: String): Tree = {

    val src: java.io.DataInputStream = new DataInputStream(this.getClass().getResourceAsStream(file))
    val bytes: List[Byte] = new XZReader(src)()
    src.close()
    val (nodeTree, rest1) = new AstDecompressor()(bytes.tail)
    val (names, rest2) = new NameDecompressor()(rest1)
    val constants = new ConstantDecompressor[u.type](u)(rest2)

    /* TODO: rebuilt the tree from each part, ASTs, Symbols, etc. */
    new TreeRecomposer[u.type](u)(DecTree(nodeTree.flattenBFSIdx, names, constants))
  }
  def getMethodDef(file: String, name: List[String]): Tree = getElement(file, name, NodeTag.DefDef)
  def getValDef(file: String, name: List[String]): Tree = getElement(file, name, NodeTag.ValDef)
  def getModuleDef(file: String, name: List[String]): Tree = getElement(file, name, NodeTag.ModuleDef)
  def getClassDef(file: String, name: List[String]): Tree = getElement(file, name, NodeTag.ClassDef)
  def getTypeDef(file: String, name: List[String]): Tree = getElement(file, name, NodeTag.TypeDef)
  def getLabelDef(file: String, name: List[String]): Tree = getElement(file, name, NodeTag.LabelDef)

  /*TODO here: The file value might be wrong here*/
  def getNewElement(file: String, fullName: List[String], tpe: NodeTag.Value): Tree = {
    val (nodeTree, flatNames, flatConstants) = nameBasedRead(file, fullName.last)
    val bfs: RevList[NodeBFS] = nodeTree.flattenBFSIdx
    val names: Map[String, List[Int]] = initNames(flatNames, bfs)
    val constants = initConstants(flatConstants, bfs)
    saved += (file -> (nodeTree, bfs, names, constants))
    val subtree: RevList[NodeBFS] = findWithFullPath(fullName, names, bfs.reverse)
    new TreeRecomposer[u.type](u)(DecTree(subtree, names, constants))
  }
  def getElement(file: String, fullName: List[String], tpe: NodeTag.Value): Tree = {
    if (!saved.contains(file)) {
      getNewElement(file, fullName, tpe)
    } else {
      val (_, bfs, names, constants) = saved(file)
      /*TODO replace here the call*/
      val subtree: RevList[NodeBFS] = findWithFullPath(fullName, names, bfs.reverse)
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
  def initConstants(constants: ConstantDict, nbfs: RevList[NodeBFS]): ConstantDict = {
    val toZip = constants.map(x => x._2.map(y => (y, x._1))).toList.flatten.sortBy(_._1)
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
  /*Finds the correct tree to reconstruct given a fullName specification*/
  /*TODO handle all the corner cases correctly with exceptions and all*/
  def findWithFullPath(fullPath: List[String], names: Map[String, List[Int]], tree: List[NodeBFS]): RevList[NodeBFS] = {

    /*Keeps only entries in the names that are defines and contained in fullPath*/
    val withDefs: Map[String, List[Int]] = fullPath.map { x =>
      val filtered: List[Int] = names(x).filter(y => NodeTag.isADefine(tree.find(z => z.bfsIdx == y).get.node.tpe))
      (x, filtered)
    }.toMap
    /*Gets the trees foreach of the defines of the start of fullPath*/
    val rootTrees: List[RevList[NodeBFS]] = withDefs(fullPath.head).map { i =>
      extractSubBFS(tree.drop(i))
    }.toList
    /*Finds the correct one*/
    val candidate: RevList[NodeBFS] = rootTrees.filter { t =>
      val (max, min) = (t.head.bfsIdx, t.last.bfsIdx)
      fullPath.tail.forall { n =>
        val indexes = withDefs(n)
        t.exists(no => indexes.contains(no.bfsIdx))
      }
    }.head
    val index: Int = withDefs(fullPath.last).find(i => candidate.exists(y => y.bfsIdx == i)).get

    extractSubBFS(tree.drop(index))
  }
  /* Helper function that reads all the elements we need to reconstruct the tree */
  def nameBasedRead(file: String, name: String): (Node, NameDict, ConstantDict) = {
    val src: java.io.DataInputStream = new DataInputStream(this.getClass().getResourceAsStream(file))
    val bytes: List[Byte] = new XZReader(src)()
    val (nodeTree, rest1) = new AstDecompressor()(bytes.tail)
    val (names, rest2) = new NameDecompressor()(rest1)
    val constants = new ConstantDecompressor[u.type](u)(rest2)
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
