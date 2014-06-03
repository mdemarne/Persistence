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

  test("Basic Tree to recompose") {
    val t1 = reify { class Flower { def name = "Hello" } }.tree
    val decTree = decomposer(t1)
    val t2 = recomposer(decTree)
    println("Original tree\n" + t1)
    println("Decompressed tree\n" + t2)
    /* Visual debugging */
  }
}