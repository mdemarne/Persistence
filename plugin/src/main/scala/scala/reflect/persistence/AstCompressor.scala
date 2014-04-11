package scala.reflect.persistence

import java.io.DataOutputStream

class AstCompressor(out: DataOutputStream) {
  import Enrichments._

  def apply(node: Node): Unit = ???
}
