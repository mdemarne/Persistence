package scala.reflect.persistence

/* Wrapper for decompsed tree (Nodes and Names) */
case class DecTree(treeBFS: List[NodeBFS], names: Map[String, List[Int]])