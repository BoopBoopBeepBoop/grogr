package grogr.core.sql

import com.criteo.vizatra.vizsql.*
import grogr.core.expr.AlgebraicForm
import grogr.core.expr.Expr.*
import grogr.core.expr.Token.{Blend, Cross, Nest}
import grogr.core.model

import scala.util.Random

object SqlGeneration {

  private val LEFT = "left"
  private val RIGHT = "right"

  def apply(algForm: AlgebraicForm)(using schema: model.IndexedSchema) = {
    given GenerationContext = GenerationContext(getRequired(algForm.symOp))
    generateSql(algForm.symOp)
  }

  case class GenerationContext(
      required: Map[model.Table, Seq[model.Column]]
  )

  case class Key(referenced: Seq[String], addl: Seq[String], tables: Set[model.Table]) {
    def ++(other: Key) =
      Key(
        (referenced ++ other.referenced).distinct,
        (addl ++ other.addl).distinct,
        tables ++ other.tables
      )
  }

  def getRequired(symOp: SymOp)(using schema: model.IndexedSchema): Map[model.Table, Seq[model.Column]] = {
    symOp.exprTraverse {
      case r: Reference => Seq(schema.column(r.value, r.table))
    }.groupMap(_._2)(_._1)
  }

  def generateSql(
      symOp: SymOp)(
      using schema: model.IndexedSchema,
      gc: GenerationContext
  ): (Key, Select) = symOp match {
    case Container(sub) =>
      generateSql(sub)

    case Operator(Cross, l, r) =>
      val (lkey, left) = generateSql(l)
      val (rkey, right) = generateSql(r)

      if ((lkey.tables diff rkey.tables).size == 0) return (rkey, right)
      else if ((rkey.tables diff lkey.tables).size == 0) return (lkey, left)

      val (relation, newTables) =
        schema
          .findJoin(lkey.tables, rkey.tables)
          .map(createJoinRelation(lkey, left, rkey, right, FullJoin))
          .getOrElse {
            noJoinPathRelation(lkey, left, rkey, right)
          }

      val (names, proj) = newTables.map(projectionsForTable).unzip
      val newNames = names.flatten.toSeq
      val newProjections = proj.flatten

      val leftRef = lkey.referenced
      val set0 = leftRef.toSet
      val rightRef = rkey.referenced.filterNot(set0.contains)
      val set1 = set0 ++ rightRef
      val leftAddl = lkey.addl.filterNot(set1.contains)
      val set2 = set1 ++ leftAddl
      val rightAddl = rkey.addl.filterNot(set2.contains)
      val set3 = set2 ++ rightAddl
      val (newAddl, newCols) = newNames.zip(newProjections).filterNot { case (n, p) => set3.contains(n) }.unzip


      // adding the extra tables here but not including required variables in the referenced section
      val passthroughCols =
        leftRef.map(subselectColAccess(LEFT)) ++ rightRef.map(subselectColAccess(RIGHT))

      val passthroughAddl =
        leftAddl.map(subselectColAccess(LEFT)) ++ rightAddl.map(subselectColAccess(RIGHT))

      val newKey = lkey ++ rkey ++ Key(Seq.empty, newAddl, newTables)

      newKey -> SimpleSelect(
        projections = (passthroughCols ++ passthroughAddl ++ newCols).toList, // ++ newProjections,
        relations = relation.toList
      )

    case Operator(Nest, l, r) =>
      val (lkey, left) = generateSql(l)
      val (rkey, right) = generateSql(r)

      if ((lkey.tables diff rkey.tables).size == 0) return (rkey, right)
      else if ((rkey.tables diff lkey.tables).size == 0) return (lkey, left)

      val (relation, newTables) =
        schema
          .findJoin(lkey.tables, rkey.tables)
          .map(createJoinRelation(lkey, left, rkey, right, InnerJoin))
          .getOrElse {
            noJoinPathRelation(lkey, left, rkey, right)
          }

      val (names, proj) = newTables.map(projectionsForTable).unzip
      val newNames = names.flatten.toSeq
      val newProjections = proj.flatten

      val leftRef = lkey.referenced
      val set0 = leftRef.toSet
      val rightRef = rkey.referenced.filterNot(set0.contains)
      val set1 = set0 ++ rightRef
      val leftAddl = lkey.addl.filterNot(set1.contains)
      val set2 = set1 ++ leftAddl
      val rightAddl = rkey.addl.filterNot(set2.contains)
      val set3 = set2 ++ rightAddl
      val (newAddl, newCols) = newNames.zip(newProjections).filterNot { case (n, p) => set3.contains(n) }.unzip


      // adding the extra tables here but not including required variables in the referenced section
      val passthroughCols =
        leftRef.map(subselectColAccess(LEFT)) ++ rightRef.map(subselectColAccess(RIGHT))

      val passthroughAddl =
        leftAddl.map(subselectColAccess(LEFT)) ++ rightAddl.map(subselectColAccess(RIGHT))

      val newKey = lkey ++ rkey ++ Key(Seq.empty, newAddl, newTables)

      newKey -> SimpleSelect(
        projections = (passthroughCols ++ passthroughAddl ++ newCols).toList, // ++ newProjections,
        relations = relation.toList
      )

    case Operator(Blend, l, r) =>
      val (lkey, left) = generateSql(l)
      val (rkey, right) = generateSql(r)

      lkey -> UnionSelect(
        left = left,
        distinct = Some(SetAll),
        right = right
      )

    case Reference(str, maybeTable, maybeSchema) =>
      // lookup the reference from our schema to see if it only applies to a single table in scope
      val (col, table) = schema.column(str, maybeTable)
      val (cols, colProj) = projectionsForTable(table)

      Key(Seq(s"${table.name}|${col.name}"), cols.toSeq, Set(table)) -> SimpleSelect(
        projections = colProj.toList,
        relations = List(SingleTableRelation(TableIdent(table.name)))
      )

    case Unity =>
      val id = unityId()
      Key(Seq(id), Seq.empty, Set(model.Table(id, List(model.Column(id, "unknown"))))) -> SimpleSelect(
        projections = List(ExpressionProjection(
          LiteralExpression(NullLiteral),
          alias = Some(s"\"$id\"")
        ))
      )
  }

  val rand = new Random
  private def unityId() = "unity|" + rand.alphanumeric.take(6).mkString.toLowerCase()

  private def projectionsForTable(
      t: model.Table)(
      using s: model.IndexedSchema,
      gc: GenerationContext
  ): (Seq[String], Seq[ExpressionProjection]) = {

    val requiredRelationColumns = s.relationCols(t)
    val (requiredProjections, requiredNames) = requiredRelationColumns.map { col =>
      val hasCanonical = s.canonicalName(t.name, col.name)

      hasCanonical.map { case (canonTable, canonCol) =>
        val plainName = s"${canonTable}|${canonCol}"
        val projection = ExpressionProjection(
          // expression reads from source
          expression = ColumnExpression(ColumnIdent(col.name, Some(TableIdent(t.name)))),
          // alias renames to canonical
          alias = Some(s"\"$plainName\"")

        )
        (projection, plainName)
      }.getOrElse {
        (prefixedColAccess(t, col.name), s"${t.name}|${col.name}")
      }
    }.unzip
    val referenced = gc.required.getOrElse(t, Seq.empty)
    val cols = referenced.map(col => s"${t.name}|${col.name}")
    (cols ++ requiredNames, referenced.map(c => prefixedColAccess(t, c.name)) ++ requiredProjections)
  }

  def subselectColAccess(from: String)(str: String) = {
    ExpressionProjection(
      expression = ColumnExpression(ColumnIdent(s"$from.\"$str\"")),
      alias = Some(s"\"$str\"")
    )
  }

  def prefixedColAccess(t: model.Table, str: String) = {
    ExpressionProjection(
      expression = ColumnExpression(ColumnIdent(str, Some(TableIdent(t.name)))),
      alias = Some(s"\"${t.name}|$str\"")
    )
  }

  def plainColAccess(s: String) = {
    ExpressionProjection(
      expression = ColumnExpression(ColumnIdent(s, None)),
      alias = None
    )
  }

  def noJoinPathRelation(
      lkey: Key,
      left: Select,
      rkey: Key,
      right: Select)(
      using schema: model.IndexedSchema,
      gc: GenerationContext
  ): (Option[Relation], Set[model.Table]) = {
    (
      Some(JoinRelation(
        SubSelectRelation(left, LEFT),
        FullJoin,
        SubSelectRelation(right, RIGHT),
        on = Some(LiteralExpression(TrueLiteral)))),
      Set.empty
    )
  }

  def createJoinRelation(
      lkey: Key,
      left: Select,
      rkey: Key,
      right: Select,
      join: Join)(
      joinPath: Seq[(model.Table, String, model.Table, String)])(
      using schema: model.IndexedSchema,
      gc: GenerationContext
  ): (Option[Relation], Set[model.Table]) = {

    val leftTable = TableIdent(LEFT)
    val rightTable = TableIdent(RIGHT)

    val newTables = joinPath
      .view
      .flatMap { case (a: model.Table, _, b: model.Table, _) => Set(a, b) }
      .filterNot(lkey.tables.contains)
      .filterNot(rkey.tables.contains)
      .toSet

    val relation: Option[Relation] = {
      joinPath
        .foldLeft[Option[Relation]](None) { case (acc: Option[Relation], (a: model.Table, acol: String, b: model.Table, bcol: String)) =>

          val includeTableName = (t: model.Table, col: String) => {
            schema.canonicalName(t.name, col)
              .map { case (canonTable, canonCol) => s"\"${canonTable}|${canonCol}\"" }
              .getOrElse(s"\"${t.name}|$col\"")
          }
          val dontIncludeTableName = (t: model.Table, col: String) => s"$col";

          val (lname, lrelation, lcolNameTransform) =
            acc.fold {
              if (lkey.tables.contains(a)) (Some(leftTable), SubSelectRelation(left, LEFT), includeTableName)
              else (Some(TableIdent(a.name)), SingleTableRelation(TableIdent(a.name)), dontIncludeTableName)
            } { prior =>
              (Some(TableIdent(a.name)), prior, dontIncludeTableName)
            }

          val (rname, rrelation, rcolNameTransform) =
            if (rkey.tables.contains(b)) (Some(rightTable), SubSelectRelation(right, RIGHT), includeTableName)
            else (Some(TableIdent(b.name)), SingleTableRelation(TableIdent(b.name)), dontIncludeTableName)

          val comparison = ComparisonExpression(
            "=",
            ColumnExpression(ColumnIdent(lcolNameTransform(a, acol), lname)),
            ColumnExpression(ColumnIdent(rcolNameTransform(b, bcol), rname))
          )

          Some(JoinRelation(lrelation, join, rrelation, on = Some(comparison)))
        }
    }

    (relation, newTables)
  }
}
