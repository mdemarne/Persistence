package scala.reflect.persistence

import scala.annotation.tailrec
import scala.reflect.internal.util.Position

/* Enum for the tree types */
object TreeTpe extends Enumeration {
  val PackageDef, ClassDef, ModuleDef, ValDef, DefDef, TypeDef, LabelDef, Import, Template, Block, CaseDef, Alternative, Star, Bind, UnApply, ArrayValue, Function, Assign, AssignOrNamedArg, If, Match, Return, Try, Throw, New, Typed, TypeApply, Apply, ApplyDynamic, This, Select, Ident, ReferenceToBoxed, Literal, Annotated, SingletonTypeTree, SelectFromTypeTree, CompoundTypeTree, AppliedTypeTree, TypeBoundsTree, ExistentialTypeTree, TypeTree, Super, EmptyTree, Separator = Value
}

/* Store an node with its bfs index and its parent bfs index */
case class NodeBFS(node: Node, bfsIdx: Int, parentBfsIdx: Int) {

  def addChild(nd: Node) = this.copy(node = this.node.copy(children = nd :: this.node.children))
}

/* Represents a node in the tree */
/* TODO: unfortunately, Constant is dependent of global. It's why it's an Any here. */
case class Node(tpe: TreeTpe.Value, children: List[Node], value: Option[Any] = None) {
  import Implicits._

  val childrenBFS: RevList[Node] = this.flattenBFS
  val childrenBFSIdx: RevList[NodeBFS] = this.flattenBFSIdx
  
  def addChildren(nd: List[Node]) = this.copy(children = nd ++ this.children)

  /* Return a subtree of 'this' with only n children, as a list of NodeBFS */
  def getSubBFS(i: Int) = childrenBFSIdx.reverse.take(i).reverse

  /*TODO never called, might be useful later*/
  /* Return a subtree of 'this' with only the n children traversed in BFS order */
  def getSubtree(i: Int): Node = getSubBFS(i).toTree.get

  /* Return a version of the node without any child */
  def cleanCopy: Node = this.copy(children = Nil)

  def flattenBFS = {
    @tailrec def loop(queue: List[Node], acc: RevList[Node]): RevList[Node] = queue match {
      case n :: ns => loop(ns ::: n.children, n.children.reverse ::: acc)
      case Nil => acc
    }
    loop(this :: Nil, this :: Nil)
  }

  /* Flatten the tree in BFS order, with nodes zipped with their indexes and their parent's indexes in BFS order */
  def flattenBFSIdx = {
    var count = 0
    def incr = { count += 1; count }
    @tailrec def loop(q: List[(Node, Int)], acc: RevList[NodeBFS]): RevList[NodeBFS] = q match {
      case Nil => acc
      case n :: ns =>
        val children = n._1.children map ((_, incr))
        loop(ns ::: children, children.map(x => NodeBFS(x._1, x._2, n._2)).reverse ::: acc)
    }
    loop((this, 0) :: Nil, NodeBFS(this, 0, -1) :: Nil)
  }

}

/* Companion object */
object Node {
  //def apply(tpe: TreeTpe.Value, children: List[Node], value: Option[Any]) = new Node(tpe, children, value)
  def apply(tpe: TreeTpe.Value, children: List[Node]) = new Node(tpe, children)
  def apply(tpe: TreeTpe.Value, value: Any) = new Node(tpe, Nil, Some(value))
  def empty = new Node(TreeTpe.EmptyTree, Nil)
  def separator = new Node(TreeTpe.Separator, Nil)
}

