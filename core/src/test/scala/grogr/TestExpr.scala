package grogr

import grogr.core.Expr.{Func, Operator, Reference}
import grogr.core.Token.{Blend, Cross, Nest}
import grogr.core.{Expr, StandardRewrite}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class TestExpr extends AnyFunSpec with Matchers {

  private val testExpr =
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

  private val testNestedFunctions =
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
  def t1(source: String, rewritten: String, terms: Seq[String]): RewriteTestCase = {
    RewriteTestCase(source, rewritten, terms)
  }

  private val rewrites = Seq(
    t1(
      source = "(A + B)/C",
      rewritten = "A / C + B / C",
      terms = Seq("A / C", "B / C")),
    t1(
      source = "A/(B+C)",
      rewritten = "A / B + A / C",
      terms = Seq("A / B", "A / C")),
    t1(
      source = "(A+(B+C))/D",
      rewritten = "A / D + B / D + C / D",
      terms = Seq("A / D", "B / D", "C / D")),
    t1(
      source = "X*(Y+Z)",
      rewritten = "X * Y + X * Z",
      terms = Seq("X * Y", "X * Z")),
    t1(
      source = "(Y+Z)*X",
      rewritten = "Y * X + Z * X",
      terms = Seq("Y * X", "Z * X")),
    t1(
      source = "((A))",
      rewritten = "(A)",
      terms = Seq("A")),
    t1(
      source = "((((A))))",
      rewritten = "(A)",
      terms = Seq("A"))
  )

  for { RewriteTestCase(input, output, terms) <- rewrites } yield {
    it(s"rewrites [$input] -> [$output]") {
      val expr = Expr(input)
      val symOps = expr.getSymOpExpressions
      symOps.size shouldEqual 1
      val op = symOps.head

      val rewritten = op.rewrite(StandardRewrite.prelimRules)

      Expr.format(rewritten) shouldEqual output
      rewritten.terms.map(Expr.format) shouldEqual terms
    }
  }

  case class FactorTestCase(source: String, termCount: Int, maxFactor: Int, last: String)
  def t2(source: String, termCount: Int, maxFactor: Int, last: String): FactorTestCase = {
    FactorTestCase(source, termCount, maxFactor, last)
  }

  private val factorAndTermTests = Seq(
    t2(
      source = "G+(A+B)*C/D",
      termCount = 3,
      maxFactor = 2,
      last = "G * 1 + A * C / D + B * C / D"
    ),
    t2(
      source = "A1*A2*A3*A4 + B",
      termCount = 2,
      maxFactor = 4,
      last = "A1 * A2 * A3 * A4 + B * 1 * 1 * 1"
    ),
    t2(
      source = "A1*A2*A3*A4 + (B + A)/Q",
      termCount = 3,
      maxFactor = 4,
      last = "A1 * A2 * A3 * A4 + B / Q * 1 * 1 * 1 + A / Q * 1 * 1 * 1"
    )
  )

  for { FactorTestCase(input, termCount, maxFactor, last) <- factorAndTermTests } yield {
    it(s"factors [$input] -> [$termCount/$maxFactor]") {
      val expr = Expr(input)
      val op = expr.getSymOpExpressions.head

      val rewritten = op.rewrite(StandardRewrite.prelimRules)
      println(s"Rewrite: $input -> ${Expr.format(rewritten)(Expr.FormatOptions(showOperatorBrackets = true))}")
      withClue("term count") {
        println("Terms: " + rewritten.terms)
        rewritten.terms.size shouldEqual termCount
      }
      withClue("max factor") {
          println("Factors: " + rewritten.terms.map(_.factors.mkString(", ")).mkString(" | "))
          rewritten.terms.map(_.factors.size).max shouldEqual maxFactor
      }
      val unityRules = StandardRewrite.unityRules(rewritten)
      val rewritten2 = rewritten.rewrite(unityRules)

      Expr.format(rewritten2) shouldEqual last
    }
  }
}
