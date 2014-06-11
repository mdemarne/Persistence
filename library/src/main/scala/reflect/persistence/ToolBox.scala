package scala.reflect.persistence

import java.io._
import scala.language.existentials
/*TODO transform most of the functions into private ones*/
/* Fetch trees and subtrees from the decompressed AST. */
class ToolBox(val u: scala.reflect.api.Universe) {
  import u._
  import Enrichments._
  var save: Map[String, (Node, RevList[NodeBFS], Map[String, List[Int]])] = Map()
 
 /*TODO this version doesn't support fullName specification yet*/
  def getSource(s: Symbol): Tree = {
    val fullPath: List[String] = s.fullName.split(".").toList
    /*TODO check that this is correct*/
    val path = s.pos.source.file.path 
    val name = fullPath.last
    /*Fully specified name for what we're looking for*/
    val fullName: List[String] = fullPath.drop(path.split("/").size).toList 
    assert(fullName.last == name) 
    
    def inner(ss: Symbol, file: String, n: String): Tree = ss.info match {
      case ModuleDef => getMethodDef(file, n)
      case ClassDef => getClassDef(file, n)
      case TypeDef => getTypeDef(file, n)
      case LabelDef => getLabelDef(file, n)
      case DefDef =>  getMethodDef(file, n)
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
    val decompressor: AstDecompressor = new AstDecompressor()
    val nodeTree = decompressor(bytes.tail)
    val toRead: List[Byte] = decompressor.getToRead 
    val names = (new NameDecompressor()(toRead))._1
    
    /* TODO: rebuilt the tree from each part, ASTs, Symbols, etc. */
    new TreeRecomposer[u.type](u)(DecTree(nodeTree.flattenBFSIdx, names))
  }

  def getMethodDef(file: String, name: String): Tree = getElement(file, name, NodeTag.DefDef) 
  def getValDef(file: String, name: String): Tree = getElement(file, name, NodeTag.ValDef)  
  def getModuleDef(file: String, name: String): Tree = getElement(file, name, NodeTag.ModuleDef)
  def getClassDef(file: String, name: String): Tree = getElement(file, name, NodeTag.ClassDef)  
  def getTypeDef(file: String, name: String): Tree = getElement(file, name, NodeTag.TypeDef)
  def getLabelDef(file: String, name: String): Tree = getElement(file, name, NodeTag.LabelDef) 

  /*TODO here: The file value might be wrong here*/
  def getNewElement(file: String, name: String, tpe: NodeTag.Value): Tree = {
    val (nodeTree, n) = nameBasedRead(file, name)
    val bfs: RevList[NodeBFS] = nodeTree.flattenBFSIdx
    val names: Map[String, List[Int]] = initNames(n, bfs)
    save +=(file -> (nodeTree, bfs, names))
    val index: Int = findIndex(bfs, tpe, names(name))
    if(index == -1)
      throw new Exception(s"Error: ${name} is not defined here")
    val subtree: List[NodeBFS] = extractSubBFS(bfs.reverse.drop(index))
    new TreeRecomposer[u.type](u)(DecTree(subtree, names))
  }

  def getElement(file: String, name: String, tpe: NodeTag.Value): Tree = {
   if(!save.contains(file)){
    getNewElement(file, name, tpe)
   }else{
    val (_, bfs, names) = save(file)
    val index: Int = findIndex(bfs, tpe, names(name))
    if(index == -1)
      throw new Exception(s"Error: ${name} is not defined here")
    val subtree: List[NodeBFS] = extractSubBFS(bfs.reverse.drop(index))
    new TreeRecomposer[u.type](u)(DecTree(subtree, names))

   }
  }
  
  def initNames(names: Map[String, List[Int]], nbfs: RevList[NodeBFS]) : Map[String, List[Int]] = {
    val toZip: List[(Int, String)] = names.map(x => x._2.map(y => (y, x._1))).toList.flatten.sortBy(_._1)
    val interm = nbfs.reverse.filter(x => NodeTag.hasAName(x.node.tpe))
    val zipped = interm.zip(toZip).map{ x => 
      (x._1.bfsIdx, x._2._2)
    }
    zipped.groupBy(_._2).map(x => (x._1, x._2.map(y => y._1) )).toMap
  }
  /* Find the bfs index of the element that corresponds to our search */
  def findIndex(nodes: RevList[NodeBFS], tpe: NodeTag.Value, occs: List[Int]): Int = occs match{
    case o::os =>
      val node: NodeBFS = nodes.find(_.bfsIdx == occs.head).get
      if(node.node.tpe == tpe){
        o
      }
      else 
        findIndex(nodes, tpe, os)
    case _ => 
      -1
  }

  /*Finds the correct tree to reconstruct given a fullName specification*/
  /*TODO handle all the corner cases correctly with exceptions and all*/
  def findWithFullPath(fullPath: List[String], names: Map[String, List[Int]], tree: List[NodeBFS]): RevList[NodeBFS] = {
   
   /*Keeps only entries in the names that are defines and contained in fullPath*/
    val withDefs: Map[String, List[Int]] = fullPath.map{ x => 
      val filtered: List[Int] = names(x).filter(y => NodeTag.isADefine(tree.find(z => z.bfsIdx == y).get.node.tpe))
      (x, filtered)
    }.toMap
    /*Gets the trees foreach of the defines of the start of fullPath*/
    val rootTrees: List[RevList[NodeBFS]] = withDefs(fullPath.head).map{ i => 
        extractSubBFS(tree.drop(i))
    }.toList
    /*Finds the correct one*/
    val candidate: RevList[NodeBFS] = rootTrees.filter{ t => 
      val (max, min) = (t.head.bfsIdx, t.last.bfsIdx)
      fullPath.tail.forall{n =>
        val indexes = withDefs(n)
        t.exists(no => indexes.contains(no.bfsIdx))
      }
    }.head
    val index: Int = withDefs(fullPath.last).find(i => candidate.exists(y => y.bfsIdx == i)).get

    extractSubBFS(tree.drop(index))
  }
  /* Helper function that reads all the elements we need to reconstruct the tree */
  def nameBasedRead(file: String, name: String): (Node, Map[String, List[Int]]) = {
    val src: java.io.DataInputStream = new DataInputStream(this.getClass().getResourceAsStream(file))
    val bytes: List[Byte] = new XZReader(src)()
    val decompressor: AstDecompressor = new AstDecompressor()
    val nodeTree: Node = decompressor(bytes.tail)
    val toRead: List[Byte] = decompressor.getToRead
    val names = (new NameDecompressor()(toRead))._1
    if(!names.contains(name))
      throw new Exception("Error: specified name doesn't exist")
    src.close()
    (nodeTree, names)
  }

  /* @warning must give the list of nodes in normal BFS and head is the node we need */
  def extractSubBFS(nodes: List[NodeBFS]): RevList[NodeBFS] = {
    assert(!nodes.isEmpty)
    def loop(nds: List[NodeBFS], acc: RevList[NodeBFS]): RevList[NodeBFS] = nds match {
      case Nil => acc
      case n::ns if(acc.head.bfsIdx > n.bfsIdx && !acc.exists(_.bfsIdx == n.parentBfsIdx)) => 
        acc
      case n::ns => 
        loop(ns, n::acc)
    }
    loop(nodes.tail, nodes.head::Nil)
  }
}
