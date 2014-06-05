package scala.reflect.persistence

/* TODO: clean imports */
import scala.tools.nsc.{ Global, Phase, SubComponent }
import scala.tools.nsc.plugins.{ Plugin => NscPlugin, PluginComponent => NscPluginComponent }
import scala.language.postfixOps
import scala.annotation.tailrec

class TreeDecomposer[U <: scala.reflect.api.Universe](val u: U) {
  import u._
  import Enrichments._
  /* Return a simplified tree along with maps of Names / Symbols / Types zipped with occurrences in BFS order */
  def apply(tree: Tree): DecTree = {
    var flatNames: List[String] = List()
    /* Traverse the tree, save names, type, symbols into corresponding list and replace them in the tree by default values*/
    @tailrec def loop(trees: List[Tree], dict: Map[Tree, Node]): Map[Tree, Node] = trees match {
      case Nil => dict
      case x :: xs =>
        val res: Node = x match {
          case PackageDef(pid, stats) =>
            Node(NodeTag.PackageDef, dict(pid) :: (stats map (dict(_))))
          case ClassDef(mods, name, tparams, impl) =>
            flatNames +:= name.toString
            Node(NodeTag.ClassDef, (tparams ::: List(impl) map (dict(_))))
          case ModuleDef(mods, name, impl) =>
            flatNames +:= name.toString
            Node(NodeTag.ModuleDef, List(dict(impl)))
          case ValDef(mods, name, tpt, rhs) =>
            flatNames +:= name.toString
            Node(NodeTag.ValDef, List(dict(tpt), dict(rhs)))
          case DefDef(mods, name, tparams, vparams, tpt, rhs) =>
            flatNames +:= name.toString
            val vnodes = vparams.map(_.map(dict(_))).flatMap(_ :+ { Node.separator })
            Node(NodeTag.DefDef, (tparams.map(dict(_)) ::: List(Node.separator) ::: vnodes ::: List(dict(tpt), dict(rhs))))
          case TypeDef(mods, name, tparams, rhs) =>
            flatNames +:= name.toString
            Node(NodeTag.TypeDef, (tparams ::: List(rhs)) map (dict(_)))
          case LabelDef(name, params, rhs) =>
            flatNames +:= name.toString
            Node(NodeTag.LabelDef, (params ::: List(rhs)) map (dict(_)))
          case Import(expr, selectors) =>
            Node(NodeTag.Import, List(dict(expr)))
          case Template(parents, self, body) =>
            Node(NodeTag.Template, (parents.map(dict(_)) ::: List(Node.separator, dict(self), Node.separator) ::: body.map(dict(_))))
          case Block(stats, expr) =>
            Node(NodeTag.Block, (stats ::: List(expr)) map (dict(_)))
          case CaseDef(pat, guard, body) =>
            Node(NodeTag.CaseDef, List(pat, guard, body) map (dict(_)))
          case Alternative(trees) =>
            Node(NodeTag.Alternative, trees map (dict(_)))
          case Star(elem) =>
            Node(NodeTag.Star, List(dict(elem)))
          case Bind(name, body) =>
            flatNames +:= name.toString
            Node(NodeTag.Bind, List(dict(body)))
          case UnApply(fun, args) =>
            Node(NodeTag.UnApply, fun :: args map (dict(_)))
          case Function(vparams, body) =>
            Node(NodeTag.Function, vparams ::: List(body) map (dict(_)))
          case Assign(lhs, rhs) =>
            Node(NodeTag.Assign, List(lhs, rhs) map (dict(_)))
          case AssignOrNamedArg(lhs, rhs) =>
            Node(NodeTag.AssignOrNamedArg, List(lhs, rhs) map (dict(_)))
          case If(cond, thenp, elsep) =>
            Node(NodeTag.If, List(cond, thenp, elsep) map (dict(_)))
          case Match(selector, cases) =>
            Node(NodeTag.Match, selector :: cases map (dict(_)))
          case Return(expr) =>
            Node(NodeTag.Return, List(dict(expr)))
          case Try(block, catches, finalizer) =>
            Node(NodeTag.Try, block :: catches ::: List(finalizer) map (dict(_)))
          case Throw(expr) =>
            Node(NodeTag.Throw, List(dict(expr)))
          case New(tpt) =>
            Node(NodeTag.New, List(dict(tpt)))
          case Typed(expr, tpt) =>
            Node(NodeTag.Typed, List(expr, tpt) map (dict(_)))
          case TypeApply(fun, args) =>
            Node(NodeTag.TypeApply, fun :: args map (dict(_)))
          case Apply(fun, args) =>
            Node(NodeTag.Apply, fun :: args map (dict(_)))
          case This(qual) =>
            flatNames +:= qual.toString
            Node(NodeTag.This, Nil)
          case Select(qualifier, selector) =>
            flatNames +:= selector.toString
            Node(NodeTag.Select, List(dict(qualifier)))
          case Ident(name) =>
            flatNames +:= name.toString
            Node(NodeTag.Ident, Nil)
          case Literal(value) =>
            Node(NodeTag.Literal)
          case Annotated(annot, arg) =>
            Node(NodeTag.Annotated, List(annot, arg) map (dict(_)))
          case SingletonTypeTree(ref) =>
            Node(NodeTag.SingletonTypeTree, List(dict(ref)))
          case SelectFromTypeTree(qualifier, selector) =>
            flatNames +:= selector.toString
            Node(NodeTag.SelectFromTypeTree, List(dict(qualifier)))
          case CompoundTypeTree(templ) =>
            Node(NodeTag.CompoundTypeTree, List(dict(templ)))
          case AppliedTypeTree(tpt, args) =>
            Node(NodeTag.AppliedTypeTree, tpt :: args map (dict(_)))
          case TypeBoundsTree(lo, hi) =>
            Node(NodeTag.TypeBoundsTree, List(lo, hi) map (dict(_)))
          case ExistentialTypeTree(tpt, whereClauses) =>
            Node(NodeTag.ExistentialTypeTree, tpt :: whereClauses map (dict(_)))
          case t: TypeTree =>
            Node(NodeTag.TypeTree, Nil)
          case Super(qual, mix) =>
            flatNames +:= mix.toString
            Node(NodeTag.Super, List(dict(qual)))
          case _ => sys.error(x.getClass().toString) /* Should never happen */
        }
        loop(xs, dict + (x -> res))
    }
    /* Generate a list of trees in BFS order */
    implicit class TreeToBFS(tree: Tree) {
      def flattenBFS = {
        @tailrec
        def loop(queue: List[Tree], acc: RevList[Tree]): RevList[Tree] = queue match {
          case expr :: exprs => loop(exprs ::: expr.children, expr.children.reverse ::: acc)
          case Nil => acc
        }
        loop(tree :: Nil, tree :: Nil)
      }
    }
    val flatTree = tree.flattenBFS
    val flatNode = loop(flatTree, Map((EmptyTree -> Node.empty)))(tree).flattenBFSIdx
    val zipNames = flatNames.zipWithIdxs
    DecTree(flatNode, zipNames)
  }
}