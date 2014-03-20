package scala.reflect.persistence

import scala.tools.nsc.{ Global, Phase, SubComponent }
import scala.tools.nsc.plugins.{ Plugin => NscPlugin, PluginComponent => NscPluginComponent }
import scala.language.postfixOps
import scala.annotation.tailrec

class Plugin(val global: Global) extends NscPlugin {
  import global._

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
        println(new RelationExpander().treeDecomposer(unit body))
        /* TODO: implement this */

        /* println(show(unit body))*/
        /* println(showRaw(unit body, printTypes = true, printIds = true, printKinds = true, printMirrors = false)) */
      }
    }

    /*Wrapper for treeDecomposer's function*/
    case class DecomposedTree(tree: Tree, namesBFS: Map[Name, List[Int]], symbBFS: Map[Symbol, List[Int]], typesBFS: Map[Type, List[Int]])
    
    class RelationExpander {
      /* Return a simplified tree along with maps of Names / Symbols / Types zipped with occurrences in BFS order */
      def treeDecomposer(tree: Tree): DecomposedTree  = {
        var nameList: List[Name] = List()
        var symbolList: List[Symbol] = List()
        var typeList: List[Type] = List()
        /* Traverse the tree, save names, type, symbols into corresponding list
         * and replace them in the tree by default values*/
        def loop(trees: List[Tree], dict: Map[Tree, Tree]): Map[Tree, Tree] = trees match {
          case Nil => dict
          case x :: xs =>
            symbolList +:= x.symbol
            typeList +:= x.tpe
            val res = x match {
              case PackageDef(pid, stats) =>
                PackageDef(dict(pid).asInstanceOf[RefTree], stats map (dict(_)))
              case ClassDef(mods, name, tparams, impl) =>
                nameList :+= name
                ClassDef(modifiersDecomposer(mods), tpnme.EMPTY, tparams map (dict(_).asInstanceOf[TypeDef]), dict(impl).asInstanceOf[Template])
              case ModuleDef(mods, name, impl) =>
                nameList :+= name
                ModuleDef(modifiersDecomposer(mods), nme.EMPTY, dict(impl).asInstanceOf[Template])
              case ValDef(mods, name, tpt, rhs) =>
                nameList :+= name
                ValDef(modifiersDecomposer(mods), nme.EMPTY, dict(tpt), dict(rhs))
              case DefDef(mods, name, tparams, vparams, tpt, rhs) =>
                nameList :+= name
                DefDef(modifiersDecomposer(mods), nme.EMPTY,
                  tparams map (dict(_).asInstanceOf[TypeDef]), vparams map (x => x map (dict(_).asInstanceOf[ValDef])),
                  dict(tpt), dict(rhs))
              case TypeDef(mods, name, tparams, rhs) =>
                nameList :+ name
                TypeDef(modifiersDecomposer(mods), tpnme.EMPTY, tparams map (dict(_).asInstanceOf[TypeDef]), dict(rhs))
              case LabelDef(name, params, rhs) =>
                nameList :+= name
                LabelDef(nme.EMPTY, params map (dict(_).asInstanceOf[Ident]), dict(rhs))
              case Import(expr, selectors) =>
                Import(dict(expr), selectors)
              case Template(parents, self, body) =>
                Template(parents map (dict(_)),
                  dict(self).asInstanceOf[ValDef], body map (dict(_)))
              case Block(stats, expr) =>
                Block(stats map (dict(_)), dict(expr))
              case CaseDef(pat, guard, body) =>
                CaseDef(dict(pat), dict(guard), dict(body))
              case Alternative(trees) =>
                Alternative(trees map (dict(_)))
              case Star(elem) =>
                Star(dict(elem))
              case Bind(name, body) =>
                nameList :+= name
                Bind(nme.EMPTY, dict(body))
              case UnApply(fun, args) =>
                UnApply(dict(fun), args map (dict(_)))
              case ArrayValue(elemtpt, elems) =>
                ArrayValue(dict(elemtpt), elems map (dict(_)))
              case Function(vparams, body) =>
                Function(vparams map (dict(_).asInstanceOf[ValDef]), dict(body))
              case Assign(lhs, rhs) =>
                Assign(dict(lhs), dict(rhs))
              case AssignOrNamedArg(lhs, rhs) =>
                AssignOrNamedArg(dict(lhs), dict(rhs))
              case If(cond, thenp, elsep) =>
                If(dict(cond), dict(thenp), dict(elsep))
              case Match(selector, cases) =>
                Match(dict(selector), cases map (dict(_).asInstanceOf[CaseDef]))
              case Return(expr) =>
                Return(dict(expr))
              case Try(block, catches, finalizer) =>
                Try(dict(block), catches map (dict(_).asInstanceOf[CaseDef]), dict(finalizer))
              case t @ Throw(expr) =>
                Throw(dict(expr))
              case n @ New(tpt) =>
                New(dict(tpt))
              case Typed(expr, tpt) =>
                Typed(dict(expr), dict(tpt))
              case TypeApply(fun, args) =>
                TypeApply(dict(fun), args map (dict(_)))
              case Apply(fun, args) =>
                Apply(dict(fun), args map (dict(_)))
              case ApplyDynamic(qual, args) =>
                ApplyDynamic(dict(qual), args map (dict(_)))
              case This(qual) =>
                This(qual)
              case Select(qualifier, selector) =>
                nameList +:= selector
                Select(dict(qualifier), nme.EMPTY)
              case Ident(name) =>
                nameList :+= name
                Ident(nme.EMPTY)
              case ReferenceToBoxed(ident) =>
                ReferenceToBoxed(dict(ident).asInstanceOf[Ident])
              case Literal(value) =>
                Literal(value) /* TODO : what do we do with values ? Can keep them as is ? */
              case Annotated(annot, arg) =>
                Annotated(dict(annot), dict(arg))
              case SingletonTypeTree(ref) =>
                SingletonTypeTree(dict(ref))
              case SelectFromTypeTree(qualifier, selector) =>
                nameList +:= selector
                SelectFromTypeTree(dict(qualifier), tpnme.EMPTY)
              case CompoundTypeTree(templ) =>
                CompoundTypeTree(dict(templ).asInstanceOf[Template])
              case AppliedTypeTree(tpt, args) =>
                AppliedTypeTree(dict(tpt), args map (dict(_)))
              case TypeBoundsTree(lo, hi) =>
                TypeBoundsTree(dict(lo), dict(hi))
              case ExistentialTypeTree(tpt, whereClauses) =>
                ExistentialTypeTree(dict(tpt), whereClauses map (dict(_).asInstanceOf[MemberDef]))
              case t: TypeTree =>
                TypeTree()
              case Super(qual, mix) =>
                nameList +:= mix
                Super(dict(qual), tpnme.EMPTY)

              case _ => sys.error(x.getClass().toString()) /* TODO : remove */
            }
            res.setPos(x.pos)
            loop(xs, dict + (x -> res))
        }
        /* generate names / symbols / types lists and simplify the annotations of modifiers */
        def modifiersDecomposer(m: Modifiers): Modifiers = {
          val mdAnnotations: List[Tree] = m.annotations map (ano => loop(ano flattenBFS, Map((EmptyTree -> EmptyTree)))(ano))
          Modifiers(m.flags, m.privateWithin, mdAnnotations)
        }
        val newTree = loop(tree flattenBFS, Map((EmptyTree -> EmptyTree)))(tree)
        DecomposedTree(newTree, nameList.zipWithIndexes, symbolList.zipWithIndexes, typeList.zipWithIndexes)
      }
      /* Generate a map of (T, List[Int]), where the values are the occurences of T in the tree in BFS order */
      implicit class BFSListToBFSMapWithIndexes[T](lst: List[T]) {
        def zipWithIndexes: Map[T, List[Int]] = lst.zipWithIndex.groupBy(v => v._1).map(e => (e._1 -> e._2.map(i => i._2)))
      }
      /* Generate a list of nodes in BFS order */
      implicit class TreeToBFS(tree: Tree) {
        def flattenBFS = {
          @tailrec
          def loop(queue: List[Tree], acc: List[Tree]): List[Tree] = queue match {
            case expr :: exprs => loop(exprs ::: expr.children, expr.children ::: acc)
            case Nil => acc
          }
          loop(tree :: Nil, tree :: Nil)
        }
      }
    }

    class SymbolCompressor { /* TODO */ }
    class TypeCompressor { /* TODO */ }
    class AstCompressor { /* TODO */ }
  }
}
