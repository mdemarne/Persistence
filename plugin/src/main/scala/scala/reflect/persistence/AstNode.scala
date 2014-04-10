package scala.reflect.persistence

import scala.annotation.tailrec
import scala.reflect.internal.util.Position

/* Enum for the tree types */
object AstTag extends Enumeration {
  val PackageDef, ClassDef, ModuleDef, ValDef, DefDef, TypeDef, LabelDef, Import, Template, Block, CaseDef, Alternative, Star, Bind, UnApply, ArrayValue, Function, Assign, AssignOrNamedArg, If, Match, Return, Try, Throw, New, Typed, TypeApply, Apply, ApplyDynamic, This, Select, Ident, ReferenceToBoxed, Literal, Annotated, SingletonTypeTree, SelectFromTypeTree, CompoundTypeTree, AppliedTypeTree, TypeBoundsTree, ExistentialTypeTree, TypeTree, Super, EmptyTree, Separator = Value
}

/* Store an node with its bfs index and its parent bfs index */
case class NodeBFS(node: Node, bfsIdx: Int, parentBfsIdx: Int) {

  def addChild(nd: Node) = this.copy(node = this.node.copy(children = nd :: this.node.children))

  def equalsStructure(that: NodeBFS): Boolean = (this.node.tpe == that.node.tpe && this.bfsIdx == that.bfsIdx && this.parentBfsIdx == that.parentBfsIdx)
}

/* Represents a node in the tree */
/* TODO: unfortunately, Constant is dependent of global. It's why it's an Any here. */
case class Node(tpe: AstTag.Value, children: List[Node], value: Option[Any] = None) {
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
  
  /* TODO: perhaps dict can be a val passed to loop */
  /* Compute a dictionary for the compression algorithm, based on LZW and the tree in BFS order */
  def computeFreqs = {
    var dict: AstDict = Map(this.getSubBFS(1) -> 0)
    @tailrec def loop(que: List[Node]): Unit = que match {
      case Nil => /* Nothing more to parse */
      case nd :: nds =>
        val bfs = nd.childrenBFSIdx
        val candidates = dict.keys.map(cand => bfs.intersectBFS(cand)).filter(_.size > 0)
        val max = candidates./:(List[NodeBFS]())((x, y) => if (x.size > y.size) x else y)
        max.size match {
          case 0 => /* nothing found, we add the new node to dict and all its children to the queue */
            dict += (bfs.takeSubtree(1) -> 1)
            loop(nds ++ nd.children)
          case sz if sz == bfs.size => /* perfect subtree matching */
            dict += (max -> (dict(max) + 1)) /* update the frequency of max in dict */
            loop(nds)
          case sz => /* the match does not cover all the subtree */
            val nwst = bfs.takeSubtree(sz + 1)
            dict += (nwst -> 1) /* add max + one more node */
            candidates foreach (cdt => dict += (cdt -> (dict(cdt) + 1))) /* update counter for all prefix trees */
            loop(nds ++ nwst.subRoots) /* add children of bfs.take(max.size + 1) to que */
        }
    }
    loop(this :: Nil)
    dict
  }

}

/* Companion object */
object Node {
  //def apply(tpe: AstTag.Value, children: List[Node], value: Option[Any]) = new Node(tpe, children, value)
  def apply(tpe: AstTag.Value, children: List[Node]) = new Node(tpe, children)
  def apply(tpe: AstTag.Value, value: Any) = new Node(tpe, Nil, Some(value))
  def empty = new Node(AstTag.EmptyTree, Nil)
  def separator = new Node(AstTag.Separator, Nil)
}

