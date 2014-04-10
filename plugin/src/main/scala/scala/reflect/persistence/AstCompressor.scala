package scala.reflect.persistence

import java.io.DataOutputStream
import scala.annotation.tailrec

class AstCompressor(out: DataOutputStream) extends (Node => Unit) {
  import Implicits._

  /* Generates the dictionary for this tree */
  def apply(node: Node): Unit = { /* TODO */ }
}
