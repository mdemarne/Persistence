package scala.reflect.persistence

import java.io._
import scala.language.existentials
/* Fetch trees and subtrees from the decompressed AST. */
class ToolBox(val u: scala.reflect.api.Universe) {
  import u._
  import Enrichments._

  type NameDict = Map[String, List[Int]]
  type ConstantDict = Map[Any, List[Int]]

  var saved: Map[String, (Node, RevList[NodeBFS], NameDict, ConstantDict)] = Map()

  /*TODO: Not sure that this function works yet, need to test it with some symbol*/ 
  def getSource(s: Symbol): Tree = {
    val fullPath: List[String] = s.fullName.split(".").toList
    val path = s.pos.source.file.path
    val name = fullPath.last
    /*Fully specified name for what we're looking for*/
    val fullName: List[String] = fullPath.drop(path.split('/').size).toList
    assert(fullName.last == name)
    def inner(ss: Symbol, file: String): Tree = ss.info match {
      case ModuleDef => getMethodDef(file, fullName)
      case ClassDef => getClassDef(file, fullName)
      case TypeDef => getTypeDef(file, fullName)
      case LabelDef => getLabelDef(file, fullName)
      case DefDef => getMethodDef(file, fullName)
      case ValDef => getValDef(file, fullName)
      case _ => throw new PersistenceException(s"Error: Type of symbol not supported, ${fullName.last} in ${file}")
    }
    inner(s, path)
  }
  
  /* General function returning the whole tree */
  def getAst(file: String): Tree = {
    val src: java.io.DataInputStream = new DataInputStream(this.getClass.getResourceAsStream("/" + file))
    val bytes: List[Byte] = new XZReader(src)()
    src.close()
    val (nodeTree, rest1) = new AstDecompressor()(bytes)
    val (names, rest2) = new NameDecompressor()(rest1)
    val constants = new ConstantDecompressor[u.type](u)(rest2)

    /* Rebuilt the tree from each part, ASTs, Symbols, etc. */
    val flatTree = nodeTree.flattenBFSIdx
    new TreeRecomposer[u.type](u)(DecTree(flatTree, initNames(names,flatTree), initConstants(constants,flatTree)))
  }
  
  /*Different specified methods to get different elements from the AST*/
  def getMethodDef(file: String, name: List[String]): Tree = getElement(file, name, NodeTag.DefDef)
  def getValDef(file: String, name: List[String]): Tree = getElement(file, name, NodeTag.ValDef)
  def getModuleDef(file: String, name: List[String]): Tree = getElement(file, name, NodeTag.ModuleDef)
  def getClassDef(file: String, name: List[String]): Tree = getElement(file, name, NodeTag.ClassDef)
  def getTypeDef(file: String, name: List[String]): Tree = getElement(file, name, NodeTag.TypeDef)
  def getLabelDef(file: String, name: List[String]): Tree = getElement(file, name, NodeTag.LabelDef)

  
  /*Method responsible for getting the AST from a new file*/
  def getNewElement(file: String, fullName: List[String], tpe: NodeTag.Value): Tree = {
    val (nodeTree, flatNames, flatConstants) = nameBasedRead(file, fullName.last)
    val bfs: RevList[NodeBFS] = nodeTree.flattenBFSIdx
    val names: Map[String, List[Int]] = initNames(flatNames, bfs)
    val constants = initConstants(flatConstants, bfs)
    saved += (file -> (nodeTree, bfs, names, constants))
    val subtree: RevList[NodeBFS] = findDefinition(fullName, names, bfs.reverse, tpe)
    new TreeRecomposer[u.type](u)(DecTree(subtree, names, constants))
  }

  /*This method looks for the specified element, if the file has not been decompressed yet, calls getNewElement*/
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
   /*Keeps only the entries in names that correspond to some definition required in the fullpath*/ 
    val defs: Map[String, List[Int]] = fullPath.map{ x => 
      val filtered: List[Int] = names(x).filter(y => NodeTag.isADefine(tree.find(z => z.bfsIdx == y).get.node.tpe))
      (x, filtered)
    }.toMap
    /*Extracts the correct tree by going through the whole path*/
    def loop(stackName: List[String], tree: List[NodeBFS]): List[NodeBFS] = stackName match {
      case n::ns =>
        /*Trees that have the correct name*/
        val roots: List[List[NodeBFS]] = defs(n).filter(i => tree.exists(nd => nd.bfsIdx == i)).map{ i => 
          extractSubBFS(tree.dropWhile(_.bfsIdx != i)).reverse
        }.toList
        /*The only one containing the total path and so the correct definition*/
        val correct: List[NodeBFS] = roots.find{ t => 
          val allDefs: Boolean = ns.forall(name => t.exists(node => defs(name).contains(node.bfsIdx)))
          /*TODO Quick hack */
          val name: String = if (ns.isEmpty) n else ns.last 
          val correctType: Boolean = t.exists(node => defs(name).contains(node.bfsIdx) && node.node.tpe == tpe)
          allDefs && correctType
        }.getOrElse(throw new PersistenceException(s"Error: findDefinition couldn't find a tree at name step ${n}"))
        loop(ns, correct)    
      case Nil => 
        tree
    }
    loop(fullPath, tree).reverse
  }

  /* Helper function that reads all the elements we need to reconstruct the tree */
  def nameBasedRead(file: String, name: String): (Node, NameDict, ConstantDict) = {
    val src: java.io.DataInputStream = new DataInputStream(this.getClass().getResourceAsStream("/" + file))
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
    def source = getSource(s)
  }
}
