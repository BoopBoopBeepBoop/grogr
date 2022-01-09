package grogr.core.expr

import com.criteo.vizatra.vizsql.*
import com.criteo.vizatra.vizsql.hive.ColumnOrStructAccessExpression
import grogr.core.expr.Expr.{Container, Operator, Reference, SymOp, Unity}
import grogr.core.expr.Token.{Blend, Cross, Nest}
import grogr.core.model.{IndexedSchema, Schema}
import grogr.core.model

object SqlGeneration {

  case class Key(cols: Set[String], tables: Set[model.Table]) {
    def ++ (other: Key) = Key(cols ++ other.cols, tables ++ other.tables)
    def ++ (more: Iterable[model.Table]) = Key(cols, tables ++ more) // doesn't actually add the columns
  }

  def generateSql(symOp: SymOp)(using schema: IndexedSchema): (Key, Select) = symOp match {
    case Container(sub) =>
      generateSql(sub)

    case Operator(Cross, l, r) =>
      val (lkey, left) = generateSql(l)
      val (rkey, right) = generateSql(r)

      val LEFT = "left"
      val RIGHT = "right"
      val leftTable = TableIdent(LEFT)
      val rightTable = TableIdent(RIGHT)

      val joinPath =
        schema.findJoin(lkey.tables, rkey.tables)
          .getOrElse { throw new RuntimeException("Join path construction failed") }

      val newTables = joinPath
        .view
        .flatMap { case (a: model.Table, _, b: model.Table, _) => Set(a, b) }
        .filterNot(lkey.tables.contains)
        .filterNot(rkey.tables.contains)
        .toSet

      val relation: Option[Relation] = {
        joinPath
          .foldLeft[Option[Relation]](None) { case (acc: Option[Relation], (a: model.Table, acol: String, b: model.Table, bcol: String)) =>
            val (lname, lrelation) =
              acc.fold {
                if (lkey.tables.contains(a)) (Some(leftTable), SubSelectRelation(left, LEFT))
                else (Some(TableIdent(a.name)), SingleTableRelation(TableIdent(a.name)))
              } { prior =>
                (Some(TableIdent(a.name)), prior)
              }

            val (rname, rrelation) =
              if (rkey.tables.contains(b)) (Some(rightTable), SubSelectRelation(right, RIGHT))
              else (Some(TableIdent(b.name)), SingleTableRelation(TableIdent(b.name)))

            val comparison = ComparisonExpression(
              "=",
              ColumnExpression(ColumnIdent(s"\"${a.name}.$acol\"", lname)),
              ColumnExpression(ColumnIdent(s"\"${b.name}.$bcol\"", rname)))

            Some(JoinRelation(lrelation, InnerJoin, rrelation, on = Some(comparison)))
          }
        }

      def colAccess(s: String) = {
        ExpressionProjection(
          expression = ColumnExpression(ColumnIdent(s, None)),
          alias = None)
      }

      (lkey ++ rkey ++ newTables) -> SimpleSelect(
        projections = (lkey.cols ++ rkey.cols).map(colAccess).toList,
        relations = relation.toList
      )

    case Operator(Nest, l, r) =>
      ???

    case Operator(Blend, l, r) =>
      ???

    case Reference(str, maybeTable, maybeSchema) =>
      // lookup the reference from our schema to see if it only applies to a single table in scope
      val (col, table) = schema.column(str, maybeTable)

      // todo: filter based on what references are actually needed?
      val requiredRelationColumns = schema.relationCols(table)

      def colAccess(s: String) = {
        ExpressionProjection(
          expression = ColumnExpression(ColumnIdent(s, None)),
          alias = Some(s"\"${table.name}.$s\""))
      }

      val cols = (requiredRelationColumns.map(_.name) + str).map(n => s"\"${table.name}.$n\"")

      Key(cols, Set(table)) -> SimpleSelect(
        projections = List(colAccess(str)) ++ requiredRelationColumns.map(c => colAccess(c.name)),
        relations = List(SingleTableRelation(TableIdent(table.name)))
      )

    case Unity =>
      Key(Set("unity"), Set.empty) -> SimpleSelect(
        projections = List(ExpressionProjection(
          LiteralExpression(IntegerLiteral(1)),
          alias = Some("unity")
        ))
      )
  }
}
