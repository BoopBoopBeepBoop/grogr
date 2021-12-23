package grogr.core.model

import upickle.default.ReadWriter

case class Schema(tables: Seq[Table], relations: Seq[Relation]) derives ReadWriter {
  override def toString = {
    s"""
      |Schema:
      |  Tables:
      |  - ${tables.mkString("  - \n")}
      |  Relations:
      |  - ${relations.mkString("  - \n")}
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
  override def toString = {
    s"$name1[$col1] -> $name2[$col2]"
  }
}
