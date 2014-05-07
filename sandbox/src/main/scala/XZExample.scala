import org.tukaani.xz.LZMA2InputStream
import java.io.{File,FileInputStream,FileOutputStream}
import org.tukaani.xz.{XZOutputStream, LZMA2Options}
import java.io.OutputStream

object XZExample {
  
  def main(args: Array[String]) {
    println("Here I am, a young man, come and get me if you can")
    val fout= new FileOutputStream("bitch.xz") 
    val comp: XZOutputStream = new XZOutputStream(fout, new LZMA2Options())
    val fin = new FileInputStream("Typers.in")
    while(fin.available() > 0){
      comp.write(fin.read());
    }
    comp.close()
    
  }

}

