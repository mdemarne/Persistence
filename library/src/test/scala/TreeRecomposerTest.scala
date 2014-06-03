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

  /* Simply decompose and recompose the tree, hence testing both the Recomposer and the Decomposer.
   * Printout - see if the normal Scala printer works on our simplified trees */
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
  test("Basic Object and Array") {
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
  test("Basic object, Lists, and nested class") {
    val t1 = reify {
      object Flower {
        case class Tulipe(xs: List[Int]) {
          def add(y: Int) = y :: xs
          def remove(y: Int) = xs.filter(x => x != y)
          def dummy = println("hello, " + x)
          def addAll(y: List[Int]) = x ++ y
          def zipAll(y: List[Boolean]) = x zip y
        }
        val x = new Tulipe(44 :: 55 :: Nil).add(22)
      }
    }.tree
    testAndPrint(t1)
  }
  test("Basic class and pattern matching") {
    val t1 = reify {
    	class Match extends ((List[Int], Boolean) => Boolean){
    	  def apply(m: (List[Int], Boolean)): Boolean = m match {
    	    case k @ (4 :: 3 :: xs, v) => if(v) println(k) else println(k._1.map(x => x*x)); v
    	    case (x :: y :: xs, true) => true
    	    case (x :: xs, _) => println("hello"); false
    	    case (Nil, v) => v
    	  } 
    	}
    }
    testAndPrint(t1)
  }
}