package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._
import scala.annotation.tailrec
import scala.reflect.persistence.Enrichments._
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.io.File
import scala.util.Random

class ToolBoxTest extends FunSuite {
  val u = scala.reflect.runtime.universe
  import u._

  /* Example source */
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

  def newPath = new File(new Random().nextInt + ".ast")
  def compress(tree: Tree, path: File) {
    val decTree = new TreeDecomposer[u.type](u)(tree)
    val compAsts = new AstCompressor()(decTree.treeBFS.toTree)
    val compNames = new NameCompressor()(decTree.names)
    new XZWriter(new DataOutputStream(new FileOutputStream(path)))(compAsts ++ compNames)
  }
  val toolbox = new ToolBox(u)

  test("toolbox fullAst1") {
	  val path = newPath
	  compress(lzwExample, path)
	  val dec = toolbox.getAst(path.getAbsolutePath())
	  println(dec)
	  /* Visual debugging */
  }
  
}
