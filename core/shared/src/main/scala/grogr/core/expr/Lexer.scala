package grogr.core.expr

import grogr.core.expr.Token
import grogr.core.expr.Token.*

import scala.util.parsing.combinator.RegexParsers

object Lexer extends RegexParsers {


  override def skipWhitespace = true

  def identifier: Parser[Identifier] = {
    "[a-zA-Z_][a-zA-Z0-9_]*".r ^^ { str => Identifier(str) }
  }

  def period = "." ^^ (_ => Period)
  def comma = "," ^^ (_ => Comma)
  def parenOpen = "(" ^^ (_ => ParenOpen)
  def parenClose = ")" ^^ (_ => ParenClose)

  def cross = "*" ^^ (_ => Cross)
  def nest = "/" ^^ (_ => Nest)
  def blend = "+" ^^ (_ => Blend)

  def operator = cross | nest | blend

  def tokens: Parser[List[Token]] = {
    phrase(rep1(identifier | operator | comma | period | parenOpen | parenClose)) ^^ { rawTokens => rawTokens }
  }

  def apply(str: String) = {
    this.parseAll(tokens, str)
  }
}
