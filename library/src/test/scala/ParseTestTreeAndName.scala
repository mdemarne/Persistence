package scala.reflect.persistence.test

import scala.reflect.persistence._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import java.io.StringReader
import scala.language.implicitConversions

object ParseTestTreeAndName extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "!")
  import Enrichments._
  val emptyName: String = "_empt_"
  case class NodeName(tpe: NodeTag.Value, name: String, var children: List[NodeName]) {
    def addChild(nd: NodeName) {
      this.children :+= nd
    }
    def addChildren(nds: List[NodeName]) {
      this.children ++= nds
    }
  }

  val strTpeMap: Map[String, NodeTag.Value] = Map(("v" -> NodeTag.ValDef), ("c" -> NodeTag.ClassDef), ("m" -> NodeTag.ModuleDef), ("t" -> NodeTag.Try), ("s" -> NodeTag.Star), ("e" -> NodeTag.ExistentialTypeTree))

  implicit def strToTpe(str: String): NodeTag.Value = strTpeMap.getOrElse(str, NodeTag.EmptyTree)

  def tpe: Parser[NodeTag.Value] = (
    ident ^^ { case e => e })

  def NodeParse: Parser[NodeName] = (
    tpe ~ ("!" ~> ident <~ "!").? ^^ {
      case e1 ~ Some(e2) => NodeName(e1, e2, Nil)
      case e1 ~ None => NodeName(e1, emptyName, Nil)
    })

  def TreeParse: Parser[NodeName] = (
    NodeParse ~ ("(" ~> rep(TreeParse) <~ ")").? ^^ {
      case e1 ~ None => e1
      case e1 ~ Some(e2) =>
        e1.addChildren(e2)
        e1
    })

  def parse(str: String): Option[(Node, Map[String, List[Int]])] = {
    val tokens = new lexical.Scanner(StreamReader(new StringReader(str)))
    phrase(TreeParse)(tokens) match {
      case Success(trees, _) =>
        return Some(toNode(trees))
      case e => println(e); None
    }
  }

  /*Translates a nodeName to a node*/
  implicit def fromNameToNode(n: NodeName): Node = Node(n.tpe, n.children)

  /*Translates a list of nodeName to a a list of node*/
  implicit def fromNamesToNodes(ns: List[NodeName]): List[Node] = ns.map(fromNameToNode(_))

  /*Goes through the tree in BFS order and generates the Node + the indexes*/
  private def toNode(ndName: NodeName): (Node, Map[String, List[Int]]) = {
    var count = 0
    var names: Map[String, List[Int]] = Map()
    def incr = { count += 1; count }
    def generateFlattenBFS(q: List[(NodeName, Int)], acc: RevList[NodeBFS]): RevList[NodeBFS] = q match {
      case Nil => acc
      case n :: ns =>
        val children: List[(NodeName, Int)] = n._1.children.map((_, incr))
        /*handles the names*/
        children.foreach { e =>
          names = updateMap(names, e._1.name, e._2, e._1.tpe)
        }
        generateFlattenBFS(ns ::: children, children.map(x => NodeBFS(x._1, x._2, n._2)).reverse ::: acc)
    }
    names = updateMap(names, ndName.name, 0, ndName.tpe)
    val res: RevList[NodeBFS] = generateFlattenBFS((ndName, 0) :: Nil, List(NodeBFS(ndName, 0, -1)))
    (res.toTree, names)
  }

  /*Enables us to update the map of strings*/
  private def updateMap(m: Map[String, List[Int]], name: String, idx: Int, tpe: NodeTag.Value): Map[String, List[Int]] = {
    if (m.contains(name)) {
      val entries: List[Int] = m(name)
      val up: List[Int] = if (NodeTag.isADefine(tpe)) idx :: entries else entries ::: List(idx)
      m + (name -> up)
    } else {
      m + (name -> List(idx))
    }
  }
}

