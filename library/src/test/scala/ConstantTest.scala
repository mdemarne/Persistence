package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._
import scala.annotation.tailrec
import scala.reflect.persistence.Enrichments._

class ConstantTest extends FunSuite {
  val u = scala.reflect.runtime.universe
  import u._

  val constantCompressor = new ConstantCompressor[u.type](u)
  val constantDecompressor = new ConstantDecompressor[u.type](u)

  def testCompAndDec(dict: Map[Any, List[Int]]) = {
    val comp = constantCompressor(dict)
    val dec = constantDecompressor(comp)
    assert(dict == dec, "Dictionary should be the same after compress + decompress")
  }

  test("testBoolean") {
    val dict: Map[Any, List[Int]] = Map((true -> List(0, 1, 2)), (false -> List(3)))
    testCompAndDec(dict)
  }
  test("testByte") {
    val dict: Map[Any, List[Int]] = Map((0x1.toByte -> List(0, 1, 2)), (0x0.toByte -> List(3)))
    testCompAndDec(dict)
  }
  test("testShort") {
    val dict: Map[Any, List[Int]] = Map((22.toShort -> List(0, 1, 2)), (34.toShort -> List(3)), (323.toShort -> List(4, 5, 6)))
    testCompAndDec(dict)
  }
  test("testChar") {
    val dict: Map[Any, List[Int]] = Map(('d' -> List(0)), ('v' -> List(1)))
    testCompAndDec(dict)
  }
  test("testInt") {
    val dict: Map[Any, List[Int]] = Map((3432 -> List(0, 1, 2)), (344 -> List(3)))
    testCompAndDec(dict)
  }
  test("testLong") {
    val dict: Map[Any, List[Int]] = Map((2434532424L -> List(0, 1, 2)), (-23213123L -> List(3)))
    testCompAndDec(dict)
  }
  test("testFloat") {
    val dict: Map[Any, List[Int]] = Map((33.2.toFloat -> List(0, 1, 2)), (-4.5.toFloat -> List(3)))
    testCompAndDec(dict)
  }
  test("testDouble") {
    val dict: Map[Any, List[Int]] = Map((33.2 -> List(0, 1, 2)), (-4.5 -> List(3)))
    testCompAndDec(dict)
  }
  test("testString") {
    val dict: Map[Any, List[Int]] = Map(
      ("Hello \n world" -> List(0, 1)),
      ("secundus \t testus" -> List(2, 3, 4)))
    testCompAndDec(dict)
  }
  test("TestCombined") {
    val dict: Map[Any, List[Int]] = Map(
      ("bb\te\nde" -> List(0, 1)),
      (5432L -> List(2, 3, 4, 5, 6, 7, 8)),
      (323.232 -> List(9, 10)),
      (0 -> List(11)),
      (0x43.toByte -> List(12)))
    testCompAndDec(dict)
  }
}