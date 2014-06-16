/**
 * Represents a decomposed tree. The tree (as a list of NodeBFS), the names
 *  and the constants are represented separately
 *
 * @author Mathieu Demarne, Adrien Ghosn
 */
package scala.reflect.persistence

case class DecTree(treeBFS: List[NodeBFS], names: Map[String, List[Int]], constants: Map[Any, List[Int]])