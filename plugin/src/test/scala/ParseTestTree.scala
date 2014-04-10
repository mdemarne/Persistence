import scala.reflect.persistence._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import java.io.StringReader
import scala.language.implicitConversions

object ParseTestTree extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")")

  /*Map for the tpe*/
  val strTpeMap: Map[String, AstTag.Value] = Map(("v" -> AstTag.ValDef),
    ("c" -> AstTag.ClassDef), ("m" -> AstTag.ModuleDef), ("t" -> AstTag.Try))

  /*Helper function to get type in parser*/
  implicit def strToTpe(str: String): AstTag.Value = strTpeMap.getOrElse(str, AstTag.EmptyTree)
  /*Parser for the tpe*/
  def tpe: Parser[AstTag.Value] = (
    ident ^^ { case e => e })

  /*Parses a simple node*/
  def NodeParse: Parser[Node] = (
    tpe ^^ { case e => Node(e) })

  /*Parses the tree*/
  def TreeParse: Parser[Node] = (
    NodeParse ~ ("(" ~> rep(TreeParse) <~ ")").? ^^ {
      case e1 ~ None => e1
      case e1 ~ Some(e2) =>
        e1.addChildren(e2)
    })

  def parse(str: String): Option[Node] = {
    val tokens = new lexical.Scanner(StreamReader(new StringReader(str)))
    phrase(TreeParse)(tokens) match {
      case Success(trees, _) =>
        return Some(trees)
      case e => println(e); None
    }
  }

  case class dictEntry(tpe: AstTag.Value, idx: Int, parentIdx: Int) {
    override def toString = s"(${tpe}, ${idx}, ${parentIdx})"
  }
  /* return an exploitable version of the dictionary from AstCompressor.parse() */
  def dictForTest(dict: Map[List[NodeBFS], Int]) = {
    var idx = 0
    dict map (x => (x._1.map(y => dictEntry(y.node.tpe, y.bfsIdx, y.parentBfsIdx)), x._2))
  }
}
