/**
 * Low-level compression: From bytes, uses the XZ library to compress them
 * into an outputstream.
 *
 *  @author Mathieu Demarne, Adrien Ghosn
 */
package scala.reflect.persistence

import java.io.DataOutputStream
import org.tukaani.xz.{ XZOutputStream, LZMA2Options, LZMA2InputStream }

class XZWriter(dest: DataOutputStream) {
  def apply(buffer: List[Byte]) {
    dest.writeLong(buffer.size)
    val comp: XZOutputStream = new XZOutputStream(dest, new LZMA2Options())
    buffer.foreach { comp.write(_) }
    comp.flush
    comp.close
  }
}