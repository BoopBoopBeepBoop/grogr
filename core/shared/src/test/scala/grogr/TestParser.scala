package grogr

import org.scalatest.funspec.AnyFunSpec
import grogr.core.Token._
import grogr.core.Expr.{Container, Func, Operator, Reference}
import grogr.core.{Expr, ExprParser, Lexer}
import org.scalatest.matchers.should.Matchers

class TestParser extends AnyFunSpec with Matchers {

  it("should parse") {
    val testCase = "foo(myPackage.thing(name1*name2 + name3/name4))"
    val Lexer.Success(out, _) = Lexer(testCase)

    out shouldEqual List(
      Identifier("foo"),
      ParenOpen,
      Identifier("myPackage"),
      Period,
      Identifier("thing"),
      ParenOpen,
      Identifier("name1"),
      Cross,
      Identifier("name2"),
      Blend,
      Identifier("name3"),
      Nest,
      Identifier("name4"),
      ParenClose,
      ParenClose
    )
  }

  val exprTests = Seq(
    (
      "name1*name2 + name3/name4" ->
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
    ),
    (
      "name1*(name2 * name3)" ->
        Operator(
          Cross,
          Reference("name1"),
          Container(Operator(
            Cross,
            Reference("name2"),
            Reference("name3"))))
    ),
    (
      "(A + B)/C" ->
          Operator(
            Nest,
            Container(
              Operator(
                Blend,
                Reference("A"),
                Reference("B"))),
            Reference("C"))
    ),
    (
      "foo(myPackage.thing(name1*name2))" ->
        Func(
          "foo",
          Seq(
            Func(
              "myPackage.thing",
              Seq(
                Operator(
                  Cross,
                  Reference("name1"),
                  Reference("name2"))))))
    )
  )

  for { (expr, expected) <- exprTests } yield {
    it(s"should parse [$expr]") {

      val Lexer.Success(out, _) = Lexer(expr)
      val ExprParser.Success(out2, _) = ExprParser(out)
      out2 shouldEqual expected
    }
  }
}
