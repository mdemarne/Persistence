package scala.reflect.persistence.test

import org.scalatest.FunSuite
import scala.reflect.persistence._
import scala.annotation.tailrec
import scala.reflect.persistence.Enrichments._

class TreeRecomposerTest extends FunSuite {
  val u = scala.reflect.runtime.universe
  import u._

  val decomposer = new TreeDecomposer[u.type](u)
  val recomposer = new TreeRecomposer[u.type](u)

  def testAndPrint(t1: Tree) {
    val decTree = decomposer(t1)
    println("decTree: " + decTree)
    println("Original tree:\n" + t1)
    val t2 = recomposer(decTree)
    println("Recomposed tree:\n" + t2)
    /* Visual debugging */
  }

  test("Basic funDef") {
    val t1 = reify { def name = "Hello" }.tree
    testAndPrint(t1)
  }

  test("Basic ClassDef1") {
    val t1 = reify { class Flower {} }.tree
    testAndPrint(t1)
  }

  test("Basic ClassDef2") {
    val t1 = reify { class Flower { def hello = "coucou"; def plus(x: Int, y: Int) = x + y } }.tree
    testAndPrint(t1)
  }

  test("Basic Object And Array") {
    val t1 = reify {
      object Flower {
        def main(args: Array[String]) {
          val v = List(1, 2, 3, 4, 5, 6)
          println(v)
          v.foreach(println(_))
        }
      }
    }.tree
    testAndPrint(t1)
  }
}