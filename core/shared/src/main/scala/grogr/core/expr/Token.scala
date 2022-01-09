package grogr.core.expr

import scala.util.parsing.input.{NoPosition, Position, Reader}

sealed trait Token
case object Token {
  case class Identifier(str: String) extends Token
  case object ExplicitUnity extends Token
  case object ParenOpen extends Token
  case object ParenClose extends Token
  case object Period extends Token
  case object Comma extends Token

  sealed trait OperatorToken extends Token
  case object Cross extends OperatorToken
  case object Nest extends OperatorToken
  case object Blend extends OperatorToken

  private [core] class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head
    override def atEnd: Boolean = tokens.isEmpty
    override def pos: Position = NoPosition
    override def rest: Reader[Token] = new TokenReader(tokens.tail)
  }
}
