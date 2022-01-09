package grogr.core.expr

import com.criteo.vizatra.vizsql.Style
import grogr.core.model.{Column, IndexedSchema, Relation, Schema, Table}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class TestSqlGen extends AnyFunSpec with Matchers {

  ///Schema:
  //  Tables:
  //  - Table[name: city, columns: [id/Int64, name/String]]
  //  - Table[name: city_groups, columns: [city_id/Int64, group/String]]
  //  - Table[name: city_groups_uniq, columns: [group/String]]
  //  - Table[name: combined, columns: [id/Int64, name/String, pop1980/Int64, pop2000/Int64, group/String]]
  //  - Table[name: pop1980, columns: [id/Int64, pop/Int64]]
  //  - Table[name: pop2000, columns: [id/Int64, pop/Int64]]

  implicit val schema: IndexedSchema = Schema(
    "default",
    tables = Seq(
      Table("city", Seq(Column("id", "Int64"), Column("name", "String"))),
      Table("city_groups", Seq(Column("city_id", "Int64"), Column("group", "String"))),
      Table("city_groups_uniq", Seq(Column("group", "String"))),
      Table("combined", Seq(Column("id", "Int64"), Column("name", "String"), Column("pop1980", "Int64"), Column("pop2000", "Int64"), Column("group", "String"))),
      Table("pop1980", Seq(Column("id", "Int64"), Column("pop", "Int64"))),
      Table("pop2000", Seq(Column("id", "Int64"), Column("pop", "Int64")))
    ),
    relations = Seq(
      Relation("city", "id", "city_groups", "city_id"),
      Relation("city_groups_uniq", "group", "city_groups", "group"),
      Relation("pop1980", "id", "city", "id"),
      Relation("pop2000", "id", "city", "id")
    )
  ).indexed


  it("should generate some sql") {
    val expr = Expr("city.name * pop1980.pop")
    println(expr)
    val symOp = expr.getSymOpExpressions.head
    println(symOp)
    val (key, sql) = SqlGeneration.generateSql(symOp)
    println(sql.show.toSQL(Style.default))
  }

  it("should generate some across tables") {
    val expr = Expr("pop2000.pop * pop1980.pop * city_groups.group")
    println(expr)
    val symOp = expr.getSymOpExpressions.head
    println(symOp)
    val (key, sql) = SqlGeneration.generateSql(symOp)
    println(sql.show.toSQL(Style.default))
  }
}
