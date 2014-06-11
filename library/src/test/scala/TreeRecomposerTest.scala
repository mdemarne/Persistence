package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._
import scala.annotation.tailrec
import scala.reflect.persistence.Enrichments._

/* TODO: won't pass, need to be update following the new mapping for names */
class TreeRecomposerTest extends FunSuite {
  val u = scala.reflect.runtime.universe
  import u._

  val decomposer = new TreeDecomposer[u.type](u)
  val recomposer = new TreeRecomposer[u.type](u)
  val toolbox = new ToolBox(u)

  /* Simply decompose and recompose the tree, hence testing both the Recomposer and the Decomposer.
   * Printout - see if the normal Scala printer works on our simplified trees */

  def testAndPrint(t1: Tree) {
    val decTree = decomposer(t1)
    println(decTree.treeBFS.map(x => (x.node.tpe, x.bfsIdx, x.parentBfsIdx)))
    println("name list:" + decTree.names)
    println("Original tree:\n" + t1)
    val t2 = recomposer(decTree.copy(names = toolbox.initNames(decTree.names, decTree.treeBFS), constants = toolbox.initConstants(decTree.constants, decTree.treeBFS)))
    println("Recomposed tree:\n" + t2)
    /* Visual debugging */
  }
  def testAndPrintSubtree(t1: Tree, idx: Int) {
    val decTree = decomposer(t1)
    println("name list:" + decTree.names)
    println("Original tree:\n" + t1)
    val newBFS = toolbox.extractSubBFS(decTree.treeBFS.reverse.drop(idx))
    println("subtree:" + newBFS.toTree)
    val t2 = recomposer(DecTree(newBFS, toolbox.initNames(decTree.names, decTree.treeBFS), toolbox.initConstants(decTree.constants, decTree.treeBFS)))
    println("Recomposed subtree:\n" + t2)
    /* Visual debugging */
  }

  /* Let's first do some simple tests */

  test("Basic funDef") {
    val t1 = reify { def name = "Hello" }.tree
    testAndPrint(t1)
  }
  test("Basic ClassDef1") {
    val t1 = reify { class Flower {} }.tree
    testAndPrint(t1)
  }
  test("Basic ClassDef2") {
    val t1 = reify { class Flower { def hello = "coucou"; def plus(x: Int, y: Int) = x + y } }.tree
    testAndPrint(t1)
  }
  test("Basic Object and Array") {
    val t1 = reify {
      object Flower {
        def main(args: Array[String]) {
          val v = List(1, 2, 3, 4, 5, 6)
          println(v)
          v.foreach(println(_))
        }
      }
    }.tree
    testAndPrint(t1)
  }
  val methodObj = reify {
    object Flower {
      case class Tulipe(xs: List[Int]) {
        def add(y: Int) = y :: xs
        def remove(y: Int) = xs.filter(x => x != y)
        def dummy = println("hello, " + x)
        def addAll(y: List[Int]) = x ++ y
        def zipAll(y: List[Boolean]) = x zip y
      }
      val x = new Tulipe(44 :: 55 :: Nil).add(22)
    }
  }.tree
  test("Basic object, Lists, and nested class") {
    testAndPrint(methodObj)
  }
  test("Basic class and pattern matching") {
    val t1 = reify {
      class Match extends ((List[Int], Boolean) => Boolean) {
        def apply(m: (List[Int], Boolean)): Boolean = m match {
          case k @ (4 :: 3 :: xs, v) =>
            if (v) println(k) else println(k._1.map(x => x * x)); v
          case (x :: y :: xs, true) => true
          case (x :: xs, _) =>
            println("hello"); false
          case (Nil, v) => v
        }
      }
    }.tree
    testAndPrint(t1)
  }

  /* Let's then have more complicated code, with subtree extraction */
  val lzwExample = reify {
    import java.io.OutputStream
    import java.io.InputStream
    import java.io.FileOutputStream
    import java.io.FileInputStream
    import java.nio.ByteBuffer
    import java.io.DataInputStream
    import java.io.DataOutputStream
    import scala.language.postfixOps
    object LzwExample {
      val initialSize = 255
      var dictC = List range (0, initialSize) map (x => (List(x.toShort) -> x.toShort)) toMap
      var dictD = List range (0, initialSize) map (x => (x.toShort -> List(x.toShort))) toMap
      def compress(in: DataInputStream, out: DataOutputStream, buffer: List[Short] = Nil): Unit = if (in.available() > 0) in.readByte.toShort match {
        case x if dictC contains (buffer :+ x) => compress(in, out, buffer :+ x)
        case x =>
          dictC += ((buffer :+ x) -> (dictC.size + 1).toShort)
          out.write(shortToArray(dictC.get(buffer).get))
          compress(in, out, x :: Nil)
      }
      else if (!buffer.isEmpty) out.write(shortToArray(dictC.get(buffer).get))
      def decompress(in: DataInputStream, out: DataOutputStream, buffer: List[Short] = Nil): Unit = if (in.available() > 0) {
        val dec = in.readShort match {
          case x if (dictD contains (x)) => dictD(x)
          case x if x == dictD.size => (buffer :+ buffer(0))
        }
        dec foreach (out.write(_))
        dictD += ((dictD.size).toShort -> (buffer :+ dec(0)))
        decompress(in, out, dec)
      }
      def shortToArray(x: Short) = {
        val buf = ByteBuffer.allocate(2)
        buf.putShort(x)
        buf.array()
      }
      def main(args: Array[String]) {
        if (args.length == 3) {
          val in = new DataInputStream(new FileInputStream(args(0)))
          val cmp = new DataOutputStream(new FileOutputStream(args(1)))
          compress(in, cmp)
          val enc = new DataInputStream(new FileInputStream(args(1)))
          val dec = new DataOutputStream(new FileOutputStream(args(2)))
          decompress(enc, dec)
        }
      }
    }

  }.tree

  test("Complicated example1") {
    testAndPrint(lzwExample)
  }
  test("subtree extraction1") {
    testAndPrintSubtree(methodObj, 9)
  }
  test("subtree extraction2") {
    testAndPrintSubtree(lzwExample, 28)
  }
  test("subtree extraction3") {
    testAndPrintSubtree(lzwExample, 121)
  }
}