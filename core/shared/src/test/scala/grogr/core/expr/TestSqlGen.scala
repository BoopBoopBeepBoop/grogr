package grogr.core.expr

import com.criteo.vizatra.vizsql.Style
import grogr.core.model.{Column, IndexedSchema, Relation, Schema, Table}
import grogr.core.sql.SqlGeneration
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

  val schema = Schema(
    "default",
    tables = Seq(
      Table("city", Seq(Column("id", "Int64"), Column("name", "String"))),
      Table("city_groups", Seq(Column("city_id", "Int64"), Column("group", "String"))),
//      Table("city_groups_uniq", Seq(Column("group", "String"))),
      Table("combined", Seq(Column("id", "Int64"), Column("name", "String"), Column("pop1980", "Int64"), Column("pop2000", "Int64"), Column("group", "String"))),
      Table("pop1980", Seq(Column("id", "Int64"), Column("pop", "Int64"))),
      Table("pop2000", Seq(Column("id", "Int64"), Column("pop", "Int64")))
    ),
    relations = Seq(
      Relation("city", "id", "city_groups", "city_id"),
//      Relation("city_groups_uniq", "group", "city_groups", "group"),
      Relation("pop1980", "id", "city", "id"),
      Relation("pop2000", "id", "city", "id")
    )
  )
  val indexed = schema.indexed


  it("should generate some sql") {
    implicit val s = indexed
    val expr = Expr("city.name * pop1980.pop")
    println(expr)
    val symOp = expr.getSymOpExpressions.head.toAlgebraicForm
    println(symOp)
    val (key, sql) = SqlGeneration(symOp)
    val gen = sql.show.toSQL(Style.default)
    println(gen)
  }

  it("should generate some across tables") {
    implicit val s = schema.copy(
      tables = schema.tables.filterNot(_.name == "combined")
    ).indexed
    val expr = Expr("city.name * pop2000.pop * group")
    println(expr)
    val symOp = expr.getSymOpExpressions.head.toAlgebraicForm
    println(symOp)
    val (key, sql) = SqlGeneration(symOp)
    println(sql.show.toSQL(Style.default))
  }

  it("should work with combined") {
    implicit val s = indexed
    val expr = Expr("combined.name * combined.pop2000 * combined.pop1980")
    println(expr)
    val symOp = expr.getSymOpExpressions.head.toAlgebraicForm
    println(symOp)
    val (key, sql) = SqlGeneration(symOp)
    println(sql.show.toSQL(Style.default))
  }

  it("should generate unity") {
    implicit val s = indexed
    val expr = Expr("combined.name * 1")
    println(expr)
    val symOp = expr.getSymOpExpressions.head.toAlgebraicForm
    println(symOp)
    val (key, sql) = SqlGeneration(symOp)
    println(sql.show.toSQL(Style.default))
  }

  it("should union") {
    implicit val s = indexed
    val expr = Expr("city_groups.group * (pop2000.pop + pop1980.pop)")
    println(expr)
    val alg = expr.getSymOpExpressions.head.toAlgebraicForm
    println(alg)
    val (key, sql) = SqlGeneration(alg)
    println(sql.show.toSQL(Style.default))
  }

  it("should nest") {
    implicit val s = indexed
    val expr = Expr("city.name / city_groups.group * pop2000.pop")
    println(expr)
    val alg = expr.getSymOpExpressions.head.toAlgebraicForm
    println(alg)
    val (key, sql) = SqlGeneration(alg)
    println(sql.show.toSQL(Style.default))
  }
}
