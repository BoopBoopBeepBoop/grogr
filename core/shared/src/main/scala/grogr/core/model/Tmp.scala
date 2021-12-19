package grogr.core.model

import scala.reflect.ClassTag

trait QueryAST {
  type Table = String
  type Schema = Vector[String]

  // relational algebra ops
  sealed abstract class Operator
  case class Scan(name: Table, schema: Schema, delim: Char, extSchema: Boolean) extends Operator
  case class PrintCSV(parent: Operator) extends Operator
  case class Project(outSchema: Schema, inSchema: Schema, parent: Operator) extends Operator
  case class Filter(pred: Predicate, parent: Operator) extends Operator
  case class Join(parent1: Operator, parent2: Operator) extends Operator
  case class Group(keys: Schema, agg: Schema, parent: Operator) extends Operator
  case class HashJoin(parent1: Operator, parent2: Operator) extends Operator

  // filter predicates
  sealed abstract class Predicate
  case class Eq(a: Ref, b: Ref) extends Predicate

  sealed abstract class Ref
  case class Field(name: String) extends Ref
  case class Value(x: Any) extends Ref

  // some smart constructors
  def Schema(schema: String*): Schema = schema.toVector
  def Scan(tableName: String): Scan = Scan(tableName, None, None)
  def Scan(tableName: String, schema: Option[Schema], delim: Option[Char]): Scan = {
    new Scan(tableName, schema.getOrElse(Vector.empty), delim.getOrElse(','), true)
  }
}


/**
SQL Parser
----------
We add a parser that takes a SQL(-like) string and converts it to tree of operators.
 */

trait SQLParser extends QueryAST {
  import scala.util.parsing.combinator._

  def parseSql(input: String) = Grammar.parseAll(input)

  object Grammar extends JavaTokenParsers with PackratParsers {

    def stm: Parser[Operator] =
      selectClause ~ fromClause ~ whereClause ~ groupClause ^^ {
        case p ~ s ~ f ~ g => g(p(f(s))) }
    def selectClause: Parser[Operator=>Operator] =
      "select" ~> ("*" ^^ { _ => (op:Operator) => op } | fieldList ^^ {
        case (fs,fs1) => Project(fs,fs1,_:Operator) })
    def fromClause: Parser[Operator] =
      "from" ~> joinClause

    def whereClause: Parser[Operator=>Operator] =
      opt("where" ~> predicate ~ andClause ^^ {
        case p ~ and => (op: Operator) => Filter(p, and(op))
      }) ^^ {
        _.getOrElse(identity)
      }

    def andClause: Parser[Operator=>Operator] =
      rep("and" ~> predicate) ^^ { list =>
        (op) => list.foldLeft(op) { case (prev, next) => Filter(next, prev) }
      }

    def groupClause: Parser[Operator=>Operator] =
      opt("group" ~> "by" ~> fieldIdList ~ ("sum" ~> fieldIdList) ^^ {
        case p1 ~ p2 => Group(p1,p2, _:Operator) }) ^^ { _.getOrElse(identity)}

    def joinClause: Parser[Operator] =
      ("nestedloops" ~> repsep(tableClause, "join") ^^ { _.reduceLeft((a,b) => Join(a,b)) }) |
        (repsep(tableClause, "join") ^^ { _.reduceLeft((a,b) => HashJoin(a,b)) })
    def tableClause: Parser[Operator] =
      tableIdent ~ opt("schema" ~> fieldIdList) ~
        opt("delim" ~> ("""\t""" ^^ (_ => '\t') | """.""".r ^^ (_.head))) ^^ {
        case table ~ schema ~ delim => Scan(table, schema, delim) } |
        ("(" ~> stm <~ ")")

    def fieldIdent: Parser[String] = """[\w\#]+""".r
    def tableIdent: Parser[String] = """[\w_\-/\.]+""".r | "?"
    def fieldList:  Parser[(Schema,Schema)] =
      repsep(fieldIdent ~ opt("as" ~> fieldIdent), ",") ^^ { fs2s =>
        val (fs,fs1) = fs2s.map { case a~b => (b.getOrElse(a),a) }.unzip
        (Schema(fs:_*),Schema(fs1:_*)) }
    def fieldIdList:  Parser[Schema] =
      repsep(fieldIdent,",") ^^ (fs => Schema(fs:_*))

    def predicate: Parser[Predicate] =
      ref ~ "=" ~ ref ^^ { case a ~ _ ~ b => Eq(a,b) }
    def ref: Parser[Ref] =
      fieldIdent ^^ Field |
        """'[^']*'""".r ^^ (s => Value(s.drop(1).dropRight(1))) |
        """[0-9]+""".r ^^ (s => Value(s.toInt))

    def parseAll(input: String): Operator = parseAll(stm,input) match {
      case Success(res,_)  => res
      case res => throw new Exception(res.toString)
    }
  }
}

object Tmp extends App with SQLParser {
  println(parseSql("select * from foo where a = b and c = 1 and d = 4"))
}