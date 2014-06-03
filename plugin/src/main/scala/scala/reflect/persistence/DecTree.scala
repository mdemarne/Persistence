package scala.reflect.persistence

/* Wrapper for decompsed tree (Nodes and Names) */
case class DecTree(tree: Node, names: Map[String, List[Int]])