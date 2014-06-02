package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._

class NamesAndTreesTest extends FunSuite {

  def compressionTest(treeStr: String) {
    val tree = ParseTestTree.parse(treeStr).get
  
  }
}
