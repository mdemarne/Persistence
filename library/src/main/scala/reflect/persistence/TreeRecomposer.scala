package scala.reflect.persistence

import scala.annotation.tailrec
import scala.language.postfixOps

/* TODO: test and adapt */
class TreeRecomposer[U <: scala.reflect.api.Universe](val u: U) {
  import u._
  import Enrichments._
  
  class NodeWrapper(nd: Node) {
    def nodeEquals(that: Node) = nd eq that
  }
  implicit class RichNodeWrapperDict(dict : Map[NodeWrapper, Tree]) {
    def apply(k: Node) = dict.find(x => x._1.nodeEquals(k)) match {
      case Some((k,n)) => n
      case None => EmptyTree
    }
  }
  def apply(decTree: DecTree): Tree = {
    var names = decTree.names.unzipWithIdxs
    var constants = decTree.constants.unzipWithIdxs
    @tailrec def loop(trees: RevList[NodeBFS], dict: Map[NodeWrapper, Tree]): Map[NodeWrapper, Tree] = trees match {
      case Nil => dict
      case NodeBFS(x, idx, _) :: xs =>
        val res = x.tpe match {
          case NodeTag.PackageDef =>
            PackageDef(dict(x.children.head).asInstanceOf[RefTree], x.children.tail map (dict(_)))
          case NodeTag.ClassDef =>
            ClassDef(NoMods, TypeName(names(idx)), x.children.init map (dict(_).asInstanceOf[TypeDef]), dict(x.children.last).asInstanceOf[Template])
          case NodeTag.ModuleDef =>
            ModuleDef(NoMods, TermName(names(idx)), dict(x.children.head).asInstanceOf[Template])
          case NodeTag.ValDef =>
            ValDef(NoMods, TermName(names(idx)), dict(x.children.head), dict(x.children.last))
          case NodeTag.DefDef =>
            val params = x.children.dropRight(2).splitOn(_ == Node.separator)
            val vparams = params.tail.map(x => x.map(dict(_).asInstanceOf[ValDef]))
            DefDef(NoMods, TermName(names(idx)), params.head.map(dict(_).asInstanceOf[TypeDef]), vparams, dict(x.children.init.last), dict(x.children.last))
          case NodeTag.TypeDef =>
            TypeDef(NoMods, TypeName(names(idx)), x.children.init map (dict(_).asInstanceOf[TypeDef]), dict(x.children.last))
          case NodeTag.LabelDef =>
            LabelDef(TermName(names(idx)), x.children.init map (dict(_).asInstanceOf[Ident]), dict(x.children.last))
          case NodeTag.Import =>
            Import(dict(x.children.head), Nil)
          case NodeTag.Template =>
            val children = x.children.splitOn(c => c.tpe == NodeTag.Separator).map(_.map(dict(_)))
            Template(children.head, children(1).head.asInstanceOf[ValDef], children.last)
          case NodeTag.Block =>
            Block(x.children.init map (dict(_)), dict(x.children.last))
          case NodeTag.CaseDef =>
            CaseDef(dict(x.children.head), dict(x.children(1)), dict(x.children.last))
          case NodeTag.Alternative =>
            Alternative(x.children map (dict(_)))
          case NodeTag.Star =>
            Star(dict(x.children.head))
          case NodeTag.Bind =>
            Bind(TermName(names(idx)), dict(x.children.head))
          case NodeTag.UnApply =>
            UnApply(dict(x.children.head), x.children.tail map (dict(_)))
          case NodeTag.Function =>
            Function(x.children.init map (dict(_).asInstanceOf[ValDef]), dict(x.children.last))
          case NodeTag.Assign =>
            Assign(dict(x.children.head), dict(x.children.last))
          case NodeTag.AssignOrNamedArg =>
            AssignOrNamedArg(dict(x.children.head), dict(x.children.last))
          case NodeTag.If =>
            If(dict(x.children.head), dict(x.children(1)), dict(x.children.last))
          case NodeTag.Match =>
            Match(dict(x.children.head), x.children.tail map (dict(_).asInstanceOf[CaseDef]))
          case NodeTag.Return =>
            Return(dict(x.children.head))
          case NodeTag.Try =>
            Try(dict(x.children.head), x.children.tail.init map (dict(_).asInstanceOf[CaseDef]), dict(x.children.last))
          case NodeTag.Throw =>
            Throw(dict(x.children.head))
          case NodeTag.New =>
            New(dict(x.children.head))
          case NodeTag.Typed =>
            Typed(dict(x.children.head), dict(x.children.last))
          case NodeTag.TypeApply =>
            def replaceTerm(tree: Tree): Tree = tree match {
		      case t: TermTree => t 
		      case b @ Bind(_: TermName, _) => b
		      case Bind(TypeName(name), rest) => Bind(TermName(name), rest)
		      case s @ Select(_, _: TermName) => s
		      case Select(start, TypeName(name)) => Select(start, TermName(name))
		      case i @ Ident(_: TermName) => i 
		      case Ident(TypeName(name)) => Ident(TermName(name))
		      case Annotated(start, arg) => Annotated(start, replaceTerm(arg))
		      case _ => sys.error("Cannot replace name from Type to Term name for TypeApply")
		    }
            TypeApply(replaceTerm(dict(x.children.head)), x.children.tail map (dict(_)))
          case NodeTag.Apply =>
            Apply(dict(x.children.head), x.children.tail map (dict(_)))
          case NodeTag.This =>
            This(TypeName(names(idx)))
          case NodeTag.Select =>
            Select(dict(x.children.head), TypeName(names(idx)))
          case NodeTag.Ident =>
            Ident(TypeName(names(idx)))
          case NodeTag.ReferenceToBoxed =>
            ReferenceToBoxed(dict(x.children.head).asInstanceOf[Ident])
          case NodeTag.Literal =>
            Literal(Constant(constants(idx)))
          case NodeTag.Annotated =>
            Annotated(dict(x.children.head), dict(x.children.last))
          case NodeTag.SingletonTypeTree =>
            SingletonTypeTree(dict(x.children.head))
          case NodeTag.SelectFromTypeTree =>
            SelectFromTypeTree(dict(x.children.head), TypeName(names(idx)))
          case NodeTag.CompoundTypeTree =>
            CompoundTypeTree(dict(x.children.head).asInstanceOf[Template])
          case NodeTag.AppliedTypeTree =>
            AppliedTypeTree(dict(x.children.head), x.children.tail map (dict(_)))
          case NodeTag.TypeBoundsTree =>
            TypeBoundsTree(dict(x.children.head), dict(x.children.last))
          case NodeTag.ExistentialTypeTree =>
            ExistentialTypeTree(dict(x.children.head), x.children.tail map (dict(_).asInstanceOf[MemberDef]))
          case NodeTag.TypeTree =>
            TypeTree()
          case NodeTag.Super =>
            Super(dict(x.children.head), TypeName(names(idx)))
          case NodeTag.EmptyTree => EmptyTree
          case _ => sys.error(x.getClass().toString()) /* Should never happen */
        }
        loop(xs, dict + (new NodeWrapper(x) -> res))
    }
    val flattenTree = decTree.treeBFS.filter(x => x.node.tpe != NodeTag.Separator && x.node.tpe != NodeTag.EmptyTree) 
    loop(flattenTree, Map())(decTree.treeBFS.last.node)
  }
}
