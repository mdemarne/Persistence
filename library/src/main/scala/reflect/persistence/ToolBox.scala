/**
 * The ToolBox is the interface with the user and is responsible
 * for getting the correct AST, for a given Symbol.
 *
 * @auhtor Adrien Ghosn, Mathieu Demarne
 */
package scala.reflect.persistence

import java.io.{ DataInputStream, FileNotFoundException }
import scala.language.existentials
/* Fetch trees and subtrees from the decompressed AST. */
class ToolBox[U <: scala.reflect.api.Universe](val u: U) {
  import u._
  import Enrichments._

  type NameDict = Map[String, List[Int]]
  type ConstantDict = Map[Any, List[Int]]

  var saved: Map[String, (Node, RevList[NodeBFS], NameDict, ConstantDict)] = Map()

  def getSource(s: Symbol, file: Option[String]): Tree = {
    val fullName: List[String] = s.fullName.split('.').toList
    val path = file match {
      case Some(sourcePath) =>
        sourcePath
      case None =>
        throw new PersistenceException("Getting ASTs back from associated file to symbol is not yet implemented. Its implementation should be fixed first.")
      /* TODO: once associated files are fixed in Symbols,  */
    }
    s match {
      case _ if s.isModule => getModuleDef(path, fullName)
      case _ if s.isClass => getClassDef(path, fullName)
      case _ if s.isType => getTypeDef(path, fullName)
      case _ if s.isMethod => getMethodDef(path, fullName)
      case _ if s.isTerm => getValDef(path, fullName)
      case _ => throw new PersistenceException(s"Error: Type of symbol not supported, ${fullName.last} in ${path}")
    }
  }

  def getMethodDef(file: String, name: List[String]): Tree = getElement(file, name, NodeTag.DefDef)
  def getValDef(file: String, name: List[String]): Tree = getElement(file, name, NodeTag.ValDef)
  def getModuleDef(file: String, name: List[String]): Tree = getElement(file, name, NodeTag.ModuleDef)
  def getClassDef(file: String, name: List[String]): Tree = getElement(file, name, NodeTag.ClassDef)
  def getTypeDef(file: String, name: List[String]): Tree = getElement(file, name, NodeTag.TypeDef)

  /* Method responsible for getting the AST from a new file */
  def getNewElement(file: String, fullName: List[String], tpe: NodeTag.Value): Tree = {
    val (nodeTree, flatNames, flatConstants) = readAstFile(file, fullName.last)
    val bfs: RevList[NodeBFS] = nodeTree.flattenBFSIdx
    val names: Map[String, List[Int]] = initNames(flatNames, bfs)
    val constants = initConstants(flatConstants, bfs)
    saved += (file -> (nodeTree, bfs, names, constants))
    val subtree: RevList[NodeBFS] = findDefinition(fullName, names, bfs.reverse, tpe)
    new TreeRecomposer[u.type](u)(DecTree(subtree, names, constants))
  }

  /* This method looks for the specified element, if the file has not been decompressed yet, calls getNewElement */
  def getElement(file: String, fullName: List[String], tpe: NodeTag.Value): Tree = {
    if (!saved.contains(file)) {
      getNewElement(file, fullName, tpe)
    } else {
      val (_, bfs, names, constants) = saved(file)
      val subtree: RevList[NodeBFS] = findDefinition(fullName, names, bfs.reverse, tpe)
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

  /*Finds the correct definition for a specified Fully named element*/
  def findDefinition(fullPath: List[String], names: Map[String, List[Int]], tree: List[NodeBFS], tpe: NodeTag.Value): RevList[NodeBFS] = {
    /* Keeps only the entries in names that correspond to some definition required in the fullpath */
    val defs: Map[String, List[Int]] = fullPath.map { x =>
      val filtered: List[Int] = names(x).filter(y => NodeTag.isADefine(tree.find(z => z.bfsIdx == y).get.node.tpe))
      (x, filtered)
    }.toMap
    /* Extracts the correct tree by going through the whole path */
    def loop(stackName: List[String], tree: List[NodeBFS]): List[NodeBFS] = stackName match {
      case n :: ns =>
        /*Trees that have the correct name*/
        val roots: List[List[NodeBFS]] = defs(n).filter(i => tree.exists(nd => nd.bfsIdx == i)).map { i =>
          extractSubBFS(tree.dropWhile(_.bfsIdx != i)).reverse
        }.toList
        /* The only one containing the total path and so the correct definition */
        val correct: List[NodeBFS] = roots.find { t =>
          val allDefs: Boolean = ns.forall(name => t.exists(node => defs(name).contains(node.bfsIdx)))
          val name: String = if (ns.isEmpty) n else ns.last
          val correctType: Boolean = t.exists(node => defs(name).contains(node.bfsIdx) && node.node.tpe == tpe)
          allDefs && correctType
        }.getOrElse(throw new PersistenceException(s"Error: findDefinition couldn't find a tree at name step: ${n}"))
        loop(ns, correct)
      case Nil =>
        tree
    }
    loop(fullPath, tree).reverse
  }

  /* Helper function that reads all the elements we need to reconstruct the tree */
  def readAstFile(file: String, name: String): (Node, NameDict, ConstantDict) = {
    val stream = this.getClass().getResourceAsStream("/" + file)
    if (stream == null) throw new FileNotFoundException("AST file does not exist: " + file)
    val src = new DataInputStream(stream)
    val bytes: List[Byte] = new XZReader(src)()
    val (nodeTree, rest1) = new AstDecompressor()(bytes)
    val (names, rest2) = new NameDecompressor()(rest1)
    val constants = new ConstantDecompressor[u.type](u)(rest2)
    if (!names.contains(name))
      throw new PersistenceException("Error: specified name doesn't exist")
    src.close()
    (nodeTree, names, constants)
  }

  /* Implicit wrapper to get a definition from a symbol */
  implicit class RichSymbol(s: Symbol) {
    def source = getSource(s, None)
    def sourceFrom(file: String) = getSource(s, Some(file))
  }
}
