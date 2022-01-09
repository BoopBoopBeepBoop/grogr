package grogr.core.model

import upickle.default.ReadWriter
import com.criteo.vizatra.vizsql as vz

case class Schema(name: String, tables: Seq[Table], relations: Seq[Relation]) derives ReadWriter {
  def indexed = IndexedSchema(this)

  override def toString = {
    s"""
      |Schema:
      |  Name: $name
      |  Tables:
      |  - ${tables.mkString("\n  - ")}
      |  Relations:
      |  - ${relations.mkString("\n  - ")}
      |""".stripMargin
  }
}
case class Table(name: String, columns: Seq[Column]) derives ReadWriter {
  override def toString = {
    s"Table[name: $name, columns: [${columns.mkString(", ")}]]"
  }
}
case class Column(name: String, `type`: String) derives ReadWriter {
  override def toString = s"""$name/${`type`}"""
}
case class Relation(name1: String, col1: String, name2: String, col2: String) derives ReadWriter {
  def invert = Relation(name2, col2, name1, col1)
  override def toString = {
    s"$name1[$col1] -> $name2[$col2]"
  }
}

class IndexedSchema(s: Schema) {
  private val _tables: Map[String, IndexedTable] =
    s.tables
      .map(t => t.name -> IndexedTable(t)).toMap

  private val _columns: Map[String, Seq[(Column, Table)]] =
    s.tables
      .flatMap(t => t.columns.map(c => c.name -> (c, t)))
      .groupMap(_._1)(_._2)

  private val _relatable: Map[String, Set[Column]] =
    s.relations
      .flatMap { r =>
        Seq(
          r.name1 -> _tables(r.name1).column(r.col1).getOrElse(throw new IllegalArgumentException(s"Column does not exist: ${r.col1}")),
          r.name2 -> _tables(r.name2).column(r.col2).getOrElse(throw new IllegalArgumentException(s"Column does not exist: ${r.col2}"))
        )
      }
      .groupMap(_._1)(_._2).map { case (k, cols) => k -> cols.toSet }

  private val g: Graph[Table] = PropGraph[Table](_.name, _.columns.map(_.name).toSet) { g =>
    s.tables.foreach(g += _)
    s.relations.foreach { rel => g(rel.name1).link(rel.col1, g(rel.name2) -> rel.col2) }
  }

  private val canonicalNames = {
    var seq = 0
    var currentMapping = Map.empty[Int, Set[(String, String)]]
    var colToMapping = Map.empty[(String, String), Int]

    s.tables
      .flatMap { t => g(t).links.map((_, t.name)) }
      .foreach { case (link, sourceTableName) =>
        val tup1 = (sourceTableName, link.source)
        val tup2 = (link.destNode.value.name, link.dest)

        val group1 = colToMapping.get(tup1)
        val group2 = colToMapping.get(tup2)

        (group1, group2) match {
          // neither are in group. Add both to a new one
          case (None, None) =>
            val newGroup = seq
            seq += 1
            colToMapping += tup1 -> newGroup
            colToMapping += tup2 -> newGroup
            currentMapping += newGroup -> Set(tup1, tup2)

          // if one is already in a group, add the other to it
          case (Some(g1), None) =>
            colToMapping += tup2 -> g1
            currentMapping += g1 -> (currentMapping(g1) + tup2)
          case (None, Some(g2)) =>
            colToMapping += tup1 -> g2
            currentMapping += g2 -> (currentMapping(g2) + tup1)

          // if both are in groups, merge them
          case (Some(g1), Some(g2)) =>
            val groupToMergeDown = currentMapping(g2)
            colToMapping ++= groupToMergeDown.map(_ -> g1).toMap
            currentMapping += (g1 -> (currentMapping(g1) ++ groupToMergeDown))
        }
      }

    // pick a canonical name for each group of variables
    val canonical = currentMapping.map { case (k, v) => k -> v.head }
    // finally build tuple -> tuple for the canonical names
    val provisional = colToMapping.map { case (tup, group) => tup -> canonical(group) }
    // and drop any canonical names from the set, so they do not return a replacement
    provisional -- canonical.map { case (_, value) => value }
  }

  val db = {
    val nullable = true
    val colTypeMapper = vz.Type.from(nullable).orElse {
      case "Int64" | "Int32" => vz.INTEGER(nullable)
      case "String" => vz.STRING(nullable)
    }
    
    val vzTables =
      s.tables
        .map { t =>
          val columns = t.columns.map { c =>
            vz.Column(c.name, colTypeMapper(c.`type`))
          }
          vz.Table(t.name, columns.toList)
        }

    vz.DB(vz.sql99.dialect, vz.Schemas(List(vz.Schema(s.name, vzTables.toList))))
  }

  def canonicalName(table: String, column: String): Option[(String, String)] = {
    canonicalNames.get((table -> column))
  }

  def relationCols(table: Table): Set[Column] = {
    _relatable.getOrElse(table.name, Set.empty)
  }

  def findJoin(t1: Set[Table], t2: Set[Table]): Option[Seq[(Table, String, Table, String)]] = {
    val shared = t1.intersect(t2)

    val left = t1.map(_.name)
    val right = t2.map(_.name)

    val joinRelationMaybe =
      if (shared.nonEmpty) {
        shared.collectFirst {
          case t: Table =>
            s.relations.find { r => r.name1 == t.name || r.name2 == t.name }
        }.flatten
      } else {
        s.relations.find { r =>
          left.contains(r.name1) && right.contains(r.name2) || left.contains(r.name2) && right.contains(r.name1)
        }
      }

    joinRelationMaybe.map { jr =>
      val ordered =
        if (right.contains(jr.name1)) jr.invert
        else jr
      val t1 = _tables(ordered.name1).t
      val t2 = _tables(ordered.name2).t
      val out: Seq[(Table, String, Table, String)] = Seq((t1, ordered.col1, t2, ordered.col2))
      out
    }.orElse {
      val foo = g.shortestPathBetweenAny(t1, t2)
      foo
    }
  }

  def column(cname: String, tname: Option[String]): (Column, Table) = {
    def noTableId: (Column, Table) = {
      val cols = _columns.getOrElse(
        cname,
        throw new IllegalArgumentException(s"Column does not exist: $cname")
      )
      if (cols.size > 1) {
        throw new IllegalArgumentException(
          s"Ambiguous column: ${cols.map { case (c, t) => s"${t.name}.${c.name}"}.mkString(", ")}"
        )
      } else cols.head
    }

    def hasTableId(tableId: String): (Column, Table) = {
      val table = _tables.getOrElse(
        tableId,
        throw new IllegalArgumentException(s"Table does not exist: $tableId")
      )
      val column =
        table
          .column(cname)
          .getOrElse(throw new IllegalArgumentException(s"Column does not exist: $tableId.$cname"))

      (column, table.t)
    }

    tname.fold(noTableId)(hasTableId)
  }
}
class IndexedTable(val t: Table) {
  private val _columns: Map[String, Column] = t.columns.map(c => c.name -> c).toMap

  def column(str: String) = _columns.get(str)
}