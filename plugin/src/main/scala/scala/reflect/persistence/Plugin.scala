package scala.reflect.persistence

import scala.tools.nsc.{ Global, Phase, SubComponent }
import scala.tools.nsc.plugins.{ Plugin => NscPlugin, PluginComponent => NscPluginComponent }
import scala.language.postfixOps
import scala.annotation.tailrec
import plugin.src.main.scala.scala.reflect.persistence.LzwCompressor
import java.io.DataOutputStream
import java.io.FileOutputStream

class Plugin(val global: Global) extends NscPlugin {
  import global._
  import Implicits._

  val name = "persistence"
  val description = """Persists typed ASTs of the entire program.
  For more information visit https://github.com/scalareflect/persistence"""
  val components = List[NscPluginComponent](PluginComponent) // Might change name

  private object PluginComponent extends NscPluginComponent {
    import global._
    val global = Plugin.this.global

    override val runsAfter = List("typer")
    val phaseName = "persist"

    def newPhase(prev: Phase) = new StdPhase(prev) {
      def apply(unit: CompilationUnit) {
        /* TODO: remove those (there for test) */
        val decomposedTree = new TreeDecomposer()(unit body)
        val recomposedTree = new TreeRecomposer()(decomposedTree)
        println("Original:")
        println(unit body)
        println("Recomposed:")
        println(recomposedTree)
        /*val lzwComp = new LzwCompressor(new DataOutputStream(new FileOutputStream("test.cmp")))*/
        /*new NameCompressor(lzwComp)(decomposedTree namesBFS)*/
        /*new SymbolCompressor(lzwComp)(decomposedTree symbBFS)*/
        /*lzwComp.flush*/
        /* TODO: implement this */
      }
    }

    /*Wrapper for treeDecomposer's function*/
    case class DecomposedTree(tree: Node, namesBFS: Map[Name, List[Int]], symbBFS: Map[Symbol, List[Int]], typesBFS: Map[Type, List[Int]])

    /* Return a simplified tree along with maps of Names / Symbols / Types zipped with occurrences in BFS order */
    class TreeDecomposer extends (Tree => DecomposedTree) {
      def apply(tree: Tree): DecomposedTree = {
        var nameList: RevList[Name] = List()
        var symbolList: RevList[Symbol] = List()
        var typeList: RevList[Type] = List()
        /* Traverse the tree, save names, type, symbols into corresponding list
         * and replace them in the tree by default values*/
        @tailrec def loop(trees: List[Tree], dict: Map[Tree, Node]): Map[Tree, Node] = trees match {
          case Nil => dict
          case x :: xs =>
            symbolList :+= x.symbol
            typeList :+= x.tpe
            val res = x match {
              case PackageDef(pid, stats) =>
                Node(TreeTpe.PackageDef, dict(pid) :: (stats map (dict(_))))
              case ClassDef(mods, name, tparams, impl) =>
                nameList :+= name
                Node(TreeTpe.ClassDef, Some(modifiersDecomposer(mods)), (tparams ::: List(impl) map (dict(_))))
              case ModuleDef(mods, name, impl) =>
                nameList :+= name
                Node(TreeTpe.ModuleDef, Some(modifiersDecomposer(mods)), List(dict(impl)))
              case ValDef(mods, name, tpt, rhs) =>
                nameList :+= name
                Node(TreeTpe.ValDef, Some(modifiersDecomposer(mods)), List(dict(tpt), dict(rhs)))
              case DefDef(mods, name, tparams, vparams, tpt, rhs) =>
                nameList :+= name
                val vnodes = vparams.map(_.map(dict(_))).flatMap(_ :+ Node.separator)
                Node(TreeTpe.DefDef, Some(modifiersDecomposer(mods)), (tparams.map(dict(_)) ::: List(Node.separator) ::: vnodes ::: List(dict(tpt), dict(rhs))))
              case TypeDef(mods, name, tparams, rhs) =>
                nameList :+ name
                Node(TreeTpe.TypeDef, Some(modifiersDecomposer(mods)), (tparams ::: List(rhs)) map (dict(_)))
              case LabelDef(name, params, rhs) =>
                nameList :+= name
                Node(TreeTpe.LabelDef, None, (params ::: List(rhs)) map (dict(_)))
              case Import(expr, selectors) =>
                Node(TreeTpe.Import, selectors map (selectorDecomposer(_)), List(dict(expr)))
              case Template(parents, self, body) =>
                Node(TreeTpe.Template, (parents.map(dict(_)) ::: List(Node.separator, dict(self), Node.separator) ::: body.map(dict(_))))
              case Block(stats, expr) =>
                Node(TreeTpe.Block, (stats ::: List(expr)) map (dict(_)))
              case CaseDef(pat, guard, body) =>
                Node(TreeTpe.CaseDef, List(pat, guard, body) map (dict(_)))
              case Alternative(trees) =>
                Node(TreeTpe.Alternative, trees map (dict(_)))
              case Star(elem) =>
                Node(TreeTpe.Star, List(dict(elem)))
              case Bind(name, body) =>
                nameList :+= name
                Node(TreeTpe.Bind, List(dict(body)))
              case UnApply(fun, args) =>
                Node(TreeTpe.UnApply, fun :: args map (dict(_)))
              case ArrayValue(elemtpt, elems) =>
                Node(TreeTpe.ArrayValue, elemtpt :: elems map (dict(_)))
              case Function(vparams, body) =>
                Node(TreeTpe.Function, vparams ::: List(body) map (dict(_)))
              case Assign(lhs, rhs) =>
                Node(TreeTpe.Assign, List(lhs, rhs) map (dict(_)))
              case AssignOrNamedArg(lhs, rhs) =>
                Node(TreeTpe.AssignOrNamedArg, List(lhs, rhs) map (dict(_)))
              case If(cond, thenp, elsep) =>
                Node(TreeTpe.If, List(cond, thenp, elsep) map (dict(_)))
              case Match(selector, cases) =>
                Node(TreeTpe.Match, selector :: cases map (dict(_)))
              case Return(expr) =>
                Node(TreeTpe.Return, List(dict(expr)))
              case Try(block, catches, finalizer) =>
                Node(TreeTpe.Try, block :: catches ::: List(finalizer) map (dict(_)))
              case Throw(expr) =>
                Node(TreeTpe.Throw, List(dict(expr)))
              case New(tpt) =>
                Node(TreeTpe.New, List(dict(tpt)))
              case Typed(expr, tpt) =>
                Node(TreeTpe.Typed, List(expr, tpt) map (dict(_)))
              case TypeApply(fun, args) =>
                Node(TreeTpe.TypeApply, fun :: args map (dict(_)))
              case Apply(fun, args) =>
                Node(TreeTpe.Apply, fun :: args map (dict(_)))
              case ApplyDynamic(qual, args) =>
                Node(TreeTpe.ApplyDynamic, qual :: args map (dict(_)))
              case This(qual) =>
                nameList :+= qual
                Node(TreeTpe.This, Nil)
              case Select(qualifier, selector) =>
                nameList :+= selector
                Node(TreeTpe.Select, List(dict(qualifier)))
              case Ident(name) =>
                nameList :+= name
                Node(TreeTpe.Ident, Nil)
              case ReferenceToBoxed(ident) =>
                Node(TreeTpe.ReferenceToBoxed, List(dict(ident)))
              case Literal(value) =>
                //Literal(value) /* TODO : what do we do with values ? Can keep them as is ? */
                Node(TreeTpe.Literal, value) //TODO HEY what do we do here ?
              case Annotated(annot, arg) =>
                Node(TreeTpe.Annotated, List(annot, arg) map (dict(_)))
              case SingletonTypeTree(ref) =>
                Node(TreeTpe.SingletonTypeTree, List(dict(ref)))
              case SelectFromTypeTree(qualifier, selector) =>
                nameList :+= selector
                Node(TreeTpe.SelectFromTypeTree, List(dict(qualifier)))
              case CompoundTypeTree(templ) =>
                Node(TreeTpe.CompoundTypeTree, List(dict(templ)))
              case AppliedTypeTree(tpt, args) =>
                Node(TreeTpe.AppliedTypeTree, tpt :: args map (dict(_)))
              case TypeBoundsTree(lo, hi) =>
                Node(TreeTpe.TypeBoundsTree, List(lo, hi) map (dict(_)))
              case ExistentialTypeTree(tpt, whereClauses) =>
                Node(TreeTpe.ExistentialTypeTree, tpt :: whereClauses map (dict(_)))
              case t: TypeTree =>
                Node(TreeTpe.TypeTree, Nil)
              case Super(qual, mix) =>
                nameList :+= mix
                Node(TreeTpe.Super, List(dict(qual)))
              case _ => sys.error(x.getClass().toString()) /* TODO : remove */
            }
            res.setPos(x.pos)
            loop(xs, dict + (x -> res))
        }
        /* generate names / symbols / types lists and simplify the annotations of modifiers */
        def modifiersDecomposer(m: Modifiers): ModifiersNode = {
          nameList :+= m.privateWithin
          val mdAnnotations: List[Node] = m.annotations map (anno => loop(anno flattenBFS, Map((EmptyTree -> Node.empty)))(anno))
          new ModifiersNode(m.flags, mdAnnotations)
        }
        /* generate a Selector node with indices */
        def selectorDecomposer(sel: ImportSelector): SelectorNode = {
          nameList ++= sel.name :: sel.rename :: Nil
          SelectorNode(sel.namePos, sel.renamePos)
        }

        val newTree = loop(tree flattenBFS, Map((EmptyTree -> Node.empty)))(tree)
        DecomposedTree(newTree, nameList.zipWithIdxs, symbolList.zipWithIdxs, typeList.zipWithIdxs)
      }
    }
    class SymbolDecomposer { /* TODO */ }

    class NameCompressor(comp: LzwCompressor) extends (Map[Name, List[Int]] => Unit) {
      /* TODO: can we have null names ? If yes, this would crash */
      def apply(namesBFS: Map[Name, List[Int]]) { /* TODO: This is just a tentative encoding as string */
        var flags: List[Int] = List()
        val nms = namesBFS.:\("") {
          (nm, acc) =>
            val p = acc + nm._1.toString + "(" + nm._2.mkString(",") + ")"
            (if (nm._1.isTermName) flags :+= 1 else flags :+= 0)
            p
        } + "#" + createTag(flags).mkString(",") + "#"
        comp(nms)
        println(nms)
      }
      /*Returns the tags grouped by 8 encoded as char*/
      def createTag(l: List[Int]): List[Char] =
        (l.grouped(8).toList).map(x => bitsToChar(x.zipWithIndex, 0))

      /*Converts sequence of bits to char*/
      def bitsToChar(l: List[(Int, Int)], acc: Byte): Char = l match {
        case Nil => acc.toChar
        case x :: xs => bitsToChar(xs, (acc + (x._1 << x._2)).toByte)
      }
    }

    class SymbolCompressor(comp: LzwCompressor) extends (Map[Symbol, List[Int]] => Unit) {
      /* TODO: find what to store here. Use pickling ? */
      /* TODO: use the LzwCompressor to compress it using the same dict as before */
      /* TODO: encode properly the symbols, e.g. as string */
      /* TODO: the names here are the same as in the AST, no need to store them twice */
      /* TODO: the types here are the same as in the AST, no need to store them twice */
      def apply(symbBFS: Map[Symbol, List[Int]]) = { ??? }
    }
    class TypeCompressor { /* TODO */ }

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

    /* Note that for test purposes, we put this class in the plugin. */
    class TreeRecomposer extends (DecomposedTree => Tree) {
      def apply(decomp: DecomposedTree): Tree = {
        var nameList: RevList[Name] = decomp.namesBFS.unzipWithIdxs
        var symbolList: RevList[Symbol] = decomp.symbBFS.unzipWithIdxs
        var typeList: RevList[Type] = decomp.typesBFS.unzipWithIdxs
        @tailrec def loop(trees: List[Node], dict: Map[Node, Tree]): Map[Node, Tree] = trees match {
          case Nil => dict
          case x :: xs =>
            val res = x.tpe match {
              case TreeTpe.PackageDef =>
                PackageDef(dict(x.children.head).asInstanceOf[RefTree], x.children.tail map (dict(_)))
              case TreeTpe.ClassDef =>
                val nm = fetchName.asInstanceOf[TypeName] /* Need to fetch name first to avoid swap with name of modifier */
                ClassDef(modifiersRecomposer(x.mods.get), nm, x.children.firsts map (dict(_).asInstanceOf[TypeDef]), dict(x.children.last).asInstanceOf[Template])
              case TreeTpe.ModuleDef =>
                val nm = fetchName.asInstanceOf[TermName]
                ModuleDef(modifiersRecomposer(x.mods.get), nm, dict(x.children.head).asInstanceOf[Template])
              case TreeTpe.ValDef =>
                val nm = fetchName.asInstanceOf[TermName]
                ValDef(modifiersRecomposer(x.mods.get), nm, dict(x.children.head), dict(x.children.last))
              case TreeTpe.DefDef =>
                val params = x.children.takeWithoutLasts(2).splitOn(_ == Node.separator)
                val vparams = params.tail.map(x => x.map(dict(_).asInstanceOf[ValDef]))
                val nm = fetchName.asInstanceOf[TermName]
                DefDef(modifiersRecomposer(x.mods.get), nm, params.head.map(dict(_).asInstanceOf[TypeDef]), vparams, dict(x.children.firsts.last), dict(x.children.last))
              case TreeTpe.TypeDef =>
                val nm = fetchName.asInstanceOf[TypeName]
                TypeDef(modifiersRecomposer(x.mods.get), nm, x.children.firsts map (dict(_).asInstanceOf[TypeDef]), dict(x.children.last))
              case TreeTpe.LabelDef =>
                LabelDef(fetchName.asInstanceOf[TermName], x.children.firsts map (dict(_).asInstanceOf[Ident]), dict(x.children.last))
              case TreeTpe.Import =>
                Import(dict(x.children.head), x.sels map (selectorRecomposer(_)))
              case TreeTpe.Template =>
                val children = x.children.splitOn(c => c.tpe == TreeTpe.Separator).map(_.map(dict(_)))
                Template(children.head, children(1).head.asInstanceOf[ValDef], children.last)
              case TreeTpe.Block =>
                Block(x.children.firsts map (dict(_)), dict(x.children.last))
              case TreeTpe.CaseDef =>
                CaseDef(dict(x.children.head), dict(x.children(1)), dict(x.children.last))
              case TreeTpe.Alternative =>
                Alternative(x.children map (dict(_)))
              case TreeTpe.Star =>
                Star(dict(x.children.head))
              case TreeTpe.Bind =>
                Bind(fetchName, dict(x.children.head))
              case TreeTpe.UnApply =>
                UnApply(dict(x.children.head), x.children.tail map (dict(_)))
              case TreeTpe.ArrayValue =>
                ArrayValue(dict(x.children.head), x.children.tail map (dict(_)))
              case TreeTpe.Function =>
                Function(x.children.firsts map (dict(_).asInstanceOf[ValDef]), dict(x.children.last))
              case TreeTpe.Assign =>
                Assign(dict(x.children.head), dict(x.children.last))
              case TreeTpe.AssignOrNamedArg =>
                AssignOrNamedArg(dict(x.children.head), dict(x.children.last))
              case TreeTpe.If =>
                If(dict(x.children.head), dict(x.children(1)), dict(x.children.last))
              case TreeTpe.Match =>
                Match(dict(x.children.head), x.children.tail map (dict(_).asInstanceOf[CaseDef]))
              case TreeTpe.Return =>
                Return(dict(x.children.head))
              case TreeTpe.Try =>
                Try(dict(x.children.head), x.children.tail.firsts map (dict(_).asInstanceOf[CaseDef]), dict(x.children.last))
              case TreeTpe.Throw =>
                Throw(dict(x.children.head))
              case TreeTpe.New =>
                New(dict(x.children.head))
              case TreeTpe.Typed =>
                Typed(dict(x.children.head), dict(x.children.last))
              case TreeTpe.TypeApply =>
                TypeApply(dict(x.children.head), x.children.tail map (dict(_)))
              case TreeTpe.Apply =>
                Apply(dict(x.children.head), x.children.tail map (dict(_)))
              case TreeTpe.ApplyDynamic =>
                ApplyDynamic(dict(x.children.head), x.children.tail map (dict(_)))
              case TreeTpe.This =>
                This(fetchName.asInstanceOf[TypeName])
              case TreeTpe.Select =>
                Select(dict(x.children.head), fetchName)
              case TreeTpe.Ident =>
                Ident(fetchName)
              case TreeTpe.ReferenceToBoxed =>
                ReferenceToBoxed(dict(x.children.head).asInstanceOf[Ident])
              case TreeTpe.Literal =>
                Literal(x.value.get.asInstanceOf[Constant]) /* TODO : value */
              case TreeTpe.Annotated =>
                Annotated(dict(x.children.head), dict(x.children.last))
              case TreeTpe.SingletonTypeTree =>
                SingletonTypeTree(dict(x.children.head))
              case TreeTpe.SelectFromTypeTree =>
                SelectFromTypeTree(dict(x.children.head), fetchName.asInstanceOf[TypeName])
              case TreeTpe.CompoundTypeTree =>
                CompoundTypeTree(dict(x.children.head).asInstanceOf[Template])
              case TreeTpe.AppliedTypeTree =>
                AppliedTypeTree(dict(x.children.head), x.children.tail map (dict(_)))
              case TreeTpe.TypeBoundsTree =>
                TypeBoundsTree(dict(x.children.head), dict(x.children.last))
              case TreeTpe.ExistentialTypeTree =>
                ExistentialTypeTree(dict(x.children.head), x.children.tail map (dict(_).asInstanceOf[MemberDef]))
              case TreeTpe.TypeTree =>
                TypeTree()
              case TreeTpe.Super =>
                Super(dict(x.children.head), fetchName.asInstanceOf[TypeName])
              case TreeTpe.EmptyTree => EmptyTree
              case _ => sys.error(x.getClass().toString()) /* TODO : remove */
            }
            /* TODO: cleaner */
            if (x.tpe != TreeTpe.EmptyTree) {
              if (typeList.head != null) res.setType(typeList.head)
              typeList = typeList.tail
              if (symbolList.head != null && x.tpe != TreeTpe.TypeTree) res.setSymbol(symbolList.head) /* TODO: cannot set symbols to TypeTree, figure out */
              symbolList = symbolList.tail
            }
            loop(xs, dict + (x -> res))
        }
        def modifiersRecomposer(m: ModifiersNode): Modifiers = {
          val privateWithin = fetchName /* necessary to avoid inversion */
          val mdAnnotations: List[Tree] = m.annotations map (anno => loop(anno flattenBFS, Map((Node.empty -> EmptyTree)))(anno))
          val ret = Modifiers(m.flags, privateWithin, mdAnnotations)
          ret
        }
        /* generate a Selector node with indices */
        def selectorRecomposer(sel: SelectorNode): ImportSelector = {
          val ret = ImportSelector(fetchName, sel.namePos, fetchName, sel.renamePos)
          ret
        }
        def fetchName = {
          val ret = nameList.head
          nameList = nameList.tail
          ret
        }
        loop(decomp.tree.flattenBFS.filter(x => x.tpe != TreeTpe.Separator), Map((Node.empty -> EmptyTree)))(decomp.tree)
      }
    }
  }
}
