package scala.reflect.persistence

import java.io.DataOutputStream

class AstCompressor(out: DataOutputStream) extends (Node => Unit) {
  import Enrichments._

  /* Generates the dictionary for this tree */
  def apply(node: Node) = ???
}
