package grogr.core

import grogr.core.Expr._
import grogr.core.Token._

import scala.util.parsing.combinator.Parsers

object ExprParser extends Parsers {
  override type Elem = Token

  private def identifier: Parser[Identifier] = {
    accept("identifier", { case id @ Identifier(name) => id })
  }

  def reference = identifier ^^ { case Identifier(name) => Reference(name) }

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
    val block = (parenthesized | reference) ~ rep(operator ~ exprBlock2) ^^ {
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
    full(reader)
  }
}
