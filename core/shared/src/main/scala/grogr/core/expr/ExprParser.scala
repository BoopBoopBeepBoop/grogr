package grogr.core.expr

import grogr.core.expr.Token.*
import grogr.core.expr.{Expr, Token}
import grogr.core.expr.Expr.*
import grogr.core.Logging
import grogr.core.expr.Expr.Reference

import scala.util.parsing.combinator.Parsers

object ExprParser extends Parsers with Logging {
  override type Elem = Token

  private def identifier: Parser[Identifier] = {
    accept("identifier", { case id@Identifier(name) => id })
  }

  def unity = {
    ExplicitUnity ^^ {
      case _ => Unity
    }
  }

  def reference = {
    identifier ~ rep(Period ~ identifier) ^^ {
      case Identifier(name) ~ list =>
        (name :: list.map(_._2.str)) match {
          case one :: Nil => Reference(one)
          case one :: two :: Nil => Reference(two, table = Some(one))
          case one :: two :: three :: Nil => Reference(three, table = Some(two), schema = Some(one))
        }
    }
  }

  def fullId: Parser[String] = {
    identifier ~ rep(Period ~ identifier) ^^ {
      case Identifier(name) ~ idList => (name +: idList.map(_._2.str)).mkString(".")
    }
  }

  def operator: Parser[OperatorToken] = {
    (Cross | Nest | Blend) ^^ {
      case foo: OperatorToken => foo
    }
  }

  def full: Parser[Expr] = {
    val fCall = (fullId ~ ParenOpen ~ full ~ rep(Comma ~ full) ~ ParenClose) ^^ {
      case id ~ _ ~ inner ~ additional ~ _ =>
        Func(id, inner +: additional.map(_._2))
    }
    fCall | exprBlock2
  }

  def exprBlock2: Parser[SymOp] = {
    val block = (parenthesized | reference | unity) ~ rep(operator ~ exprBlock2) ^^ {
      case left ~ tail =>
        tail.foldLeft[SymOp](left) {
          case (t1, op ~ t2) =>
            Operator(op, t1, t2)
        }
    }
    block
  }

  def parenthesized = ParenOpen ~ exprBlock2 ~ ParenClose ^^ {
    case _ ~ thing ~ _ => Container(thing)
  }

  def apply(tokens: List[Token]) = {
    val reader = new Token.TokenReader(tokens)
    full(reader) match {
      case Success(res, n) =>
        if (!n.atEnd) throw new RuntimeException(s"Parser had unused tokens after reading: $res")
        res
      case Failure(msg, n) => throw new RuntimeException("Parser failed: " + msg)
      case Error(msg, n) => throw new RuntimeException("Parser error: " + msg)
    }
  }
}
