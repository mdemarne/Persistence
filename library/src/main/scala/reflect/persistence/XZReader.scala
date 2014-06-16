/** Simply reads from the given InputStream and gets
  * the decompressed bytes that will be used to reconstruct the tree
  *
  * @author Adrien Ghosn, Mathieu Demarne
  */
package scala.reflect.persistence

import java.io.DataInputStream
import org.tukaani.xz.SingleXZInputStream

class XZReader(src: DataInputStream) {
  def apply(): List[Byte] = {
    val total: Long = src.readLong
    val decomp: SingleXZInputStream = new SingleXZInputStream(src)
    (1.toLong to total).map{ x => 
      (decomp.read()).toByte
    }.toList
  }
}
