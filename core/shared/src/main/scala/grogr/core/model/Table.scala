package grogr.core.model

case class Schema(tables: Seq[Table], relations: Seq[Relation]) {
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
case class Table(name: String, columns: Seq[Column]) {
  override def toString = {
    s"Table[name: $name, columns: [${columns.mkString(", ")}]]"
  }
}
case class Column(name: String) {
  override def toString = name
}
case class Relation(name1: String, col1: String, name2: String, col2: String) {
  override def toString = {
    s"$name1[$col1] -> $name2[$col2]"
  }
}
