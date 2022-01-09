package grogr.core.model


case class NodeBuilder[T](value: T) {
  private [model] var links = Set.empty[LinkRef[T]]

  def link(from: String, to: (NodeBuilder[T], String)) = {
    this.links = this.links + LinkRef(from, to._2, to._1)
    to._1.links = to._1.links + LinkRef(to._2, from, this)
  }
}

case class LinkRef[T](source: String, dest: String, destNode: NodeBuilder[T])

object PropGraph {
  def apply[T](
      idf: T => String,
      props: T => Set[String])(
      f: GraphBuilder[T] => Unit
  ) = {
    val builder = new GraphBuilder[T](idf, props)
    f(builder)
    builder.seal
  }
}

// mutable
class GraphBuilder[T](
    idf: T => String,
    props: T => Set[String]
) {
  private var nodes = Map.empty[String, NodeBuilder[T]]

  def += (value: T) = {
    val key = idf(value)
    nodes += (key -> NodeBuilder(value))
  }

  def apply(thing: T): NodeBuilder[T] = nodes(idf(thing))
  def apply(name: String): NodeBuilder[T] = nodes(name)

  private [model] def seal = Graph(idf, props, nodes)
}

class Graph[T](
    idf: T => String,
    props: T => Set[String],
    nodes: Map[String, NodeBuilder[T]]
) {
  def apply(thing: T): NodeBuilder[T] = nodes(idf(thing))
  def apply(name: String): NodeBuilder[T] = nodes(name)

  def shortestPathBetween(
      thing1: T,
      thing2: T,
      acc: Seq[(T, String, T, String)] = Seq.empty,
      cycleDetection: Set[String] = Set.empty
  ): Option[Seq[(T, String, T, String)]] = {
    val next = new scala.collection.mutable.ListBuffer[(T, String, T, String)]()
//    println(s"one: $thing1 two: $thing2 accSize: ${acc.size}")

    nodes(idf(thing1)).links.map { linkref =>
      val foo = (thing1, linkref.source, linkref.destNode.value, linkref.dest)
      if (linkref.destNode.value == thing2) {
        return Some(acc :+ foo)
      } else if (!cycleDetection.contains(idf(linkref.destNode.value))) {
        next += foo
      }
    }
    if (next.isEmpty) None
    else next.flatMap { link =>
      shortestPathBetween(link._3, thing2, acc :+ link, cycleDetection + idf(thing1))
    }.headOption
  }

  def shortestPathBetweenAny(
      thing1: Set[T],
      thing2: Set[T],
      acc: Seq[(T, String, T, String)] = Seq.empty,
      cycleDetection: Set[String] = Set.empty
  ): Option[Seq[(T, String, T, String)]] = {
    val next = new scala.collection.mutable.ListBuffer[(T, String, T, String)]()
    println(s"one: $thing1 two: $thing2 accSize: ${acc.size}")

    val searchStart = thing1.map { v =>
      val root = nodes(idf(v))
      root.links.map { linkref =>
        val foo = (root.value, linkref.source, linkref.destNode.value, linkref.dest)
        if (thing2.contains(linkref.destNode.value)) {
          return Some(acc :+ foo)
        } else if (!cycleDetection.contains(idf(linkref.destNode.value))) {
          next += foo
        }
      }
    }

    if (next.isEmpty) None
    else next.flatMap { link =>
      shortestPathBetweenAny(Set(link._3), thing2, acc :+ link, cycleDetection ++ thing1.map(idf))
    }.headOption
  }

}


