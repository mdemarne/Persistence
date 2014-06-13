package scala.reflect.persistence

/* TODO: clean imports */
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.io.File
import org.tukaani.xz.LZMA2InputStream
import java.io.{ File, FileInputStream, FileOutputStream }
import org.tukaani.xz.{ XZOutputStream, LZMA2Options }

class XZWriter(dest: DataOutputStream) {
    def apply(buffer: List[Byte]) {
      dest.writeLong(buffer.size)
      val comp: XZOutputStream = new XZOutputStream(dest, new LZMA2Options())
      buffer.foreach { comp.write(_) }
      comp.flush
      comp.close
    }
}