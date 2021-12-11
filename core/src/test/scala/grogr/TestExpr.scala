package grogr

import grogr.core.Expr.{Container, Func, Operator, Reference}
import grogr.core.{Expr, ExprParser}
import grogr.core.Token.{Blend, Cross, Nest}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class TestExpr extends AnyFunSpec with Matchers {

  val testExpr =
    Operator(
      Cross,
      Reference("name1"),
      Operator(
        Blend,
        Reference("name2"),
        Operator(
          Nest,
          Reference("name3"),
          Reference("name4"))))

  val testNestedFunctions =
    Func(
      "foo",
      Seq(
        Func(
          "myPackage.thing",
          Seq(
            Operator(
              Cross,
              Reference("name1"),
              Reference("name2")))),
        Reference("thing")
      ))

  it("should traverse") {
    testExpr.findAny { case Operator(Nest, _, _) => true } shouldEqual true
  }

  it("should get sym ops") {
    testNestedFunctions.getSymOpExpressions shouldEqual Seq(
      Operator(
        Cross,
        Reference("name1"),
        Reference("name2")),
      Reference("thing")
    )
  }

  it("should reduce to identity if requested is a sym op already") {
    val input = Operator(
      Cross,
      Reference("name1"),
      Reference("name2"))

    val output = input.getSymOpExpressions
    output.size shouldEqual 1
    output.head shouldEqual input
  }

  case class RewriteTestCase(source: String, rewritten: String, terms: Seq[String])
  def t(source: String, rewritten: String, terms: Seq[String]) = RewriteTestCase(source, rewritten, terms)  

  val rewrites = Seq(
    t(
      source = "(A + B)/C",
      rewritten = "A / C + B / C",
      terms = Seq("A / C", "B / C")),
    t(
      source = "A/(B+C)",
      rewritten = "A / B + A / C",
      terms = Seq("A / B", "A / C")),
    t(
      source = "(A+(B+C))/D",
      rewritten = "A / D + B / D + C / D",
      terms = Seq("A / D", "B / D", "C / D")),
    t(
      source = "X*(Y+Z)",
      rewritten = "X * Y + X * Z",
      terms = Seq("X * Y", "X * Z")),
    t(
      source = "(Y+Z)*X",
      rewritten = "Y * X + Z * X",
      terms = Seq("Y * X", "Z * X"))
  )

  for { RewriteTestCase(input, output, terms) <- rewrites } yield {
    it(s"rewrites [$input] -> [$output]") {
      val expr = Expr(input)
      val symOps = expr.getSymOpExpressions
      symOps.size shouldEqual 1
      val op = symOps.head

      val rewritten = op.rewrite {
        case Operator(Nest, Container(Operator(Blend, left, right)), term) =>
          Operator(Blend, Operator(Nest, left, term), Operator(Nest, right, term))
        case Operator(Nest, term, Container(Operator(Blend, left, right))) =>
          Operator(Blend, Operator(Nest, term, left), Operator(Nest, term, right))
        case Operator(Cross, Container(Operator(Blend, left, right)), term) =>
          Operator(Blend, Operator(Cross, left, term), Operator(Cross, right, term))
        case Operator(Cross, term, Container(Operator(Blend, left, right))) =>
          Operator(Blend, Operator(Cross, term, left), Operator(Cross, term, right))
      }

      Expr.format(rewritten) shouldEqual output
      rewritten.terms.map(Expr.format) shouldEqual terms
    }
  }
}
