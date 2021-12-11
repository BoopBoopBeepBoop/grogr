package grogr.core

import grogr.core.Expr.{Container, Func, Operator, Reference, SymOp, Unity}
import grogr.core.Token.{Blend, Cross, Nest, OperatorToken}


sealed trait Expr {
  def getSymOpExpressions: Seq[SymOp] = {
    traverseUntil {
      case symOp: SymOp => (false, Seq(symOp))
    }
  }

  def traverseUntil[T](f: PartialFunction[Expr, (Boolean, Seq[T])]): Seq[T] = {
      val (continue, thing) =
        if (f.isDefinedAt(this)) f(this)
        else (true, Seq.empty)

      this match {
        case _: Reference => thing
        case fc: Func =>
          thing ++ (
            if (continue) fc.arguments.flatMap(_.traverseUntil(f))
            else Nil
          )
        case c: Container =>
          thing ++ (
            if (continue) c.sub.traverseUntil(f)
            else Nil
          )
        case o: Operator =>
          thing ++ (
            if (continue) o.left.traverseUntil(f) ++ o.right.traverseUntil(f)
            else Nil
          )
        case Unity => thing
      }
  }
}

object Expr {
  def apply(str: String): Expr = {
    val Lexer.Success(out, _) = Lexer(str)
    val ExprParser.Success(out2, _) = ExprParser(out)
    out2
  }

  sealed trait SymOp extends Expr {
    def isTerm: Boolean = findAny { case Operator(Blend, _, _) => false }
    def isFactor: Boolean = findAny { case Operator(Cross, _, _) => false }

    def terms: Seq[SymOp] = {
      this match {
        case r: Reference => Seq(r)
        case c: Container => c.sub.terms
        case Operator(Blend, left, right) => left.terms ++ right.terms
        case o: Operator => Seq(o)
        case Unity => Seq(Unity)
      }
    }

    def rewrite(f: PartialFunction[SymOp, SymOp]): SymOp = {
      val out = if (f.isDefinedAt(this)) f(this) else this

      out match {
        case r: Reference => r
        case c: Container => c.copy(c.sub.rewrite(f))
        case o: Operator => o.copy(left = o.left.rewrite(f), right = o.right.rewrite(f))
        case Unity => Unity
      }
    }

    def exprTraverse[T](f: PartialFunction[SymOp, Seq[T]]): Seq[T] = {
      val thing =
        if (f.isDefinedAt(this)) f(this)
        else Seq.empty

      this match {
        case _: Reference => thing
        case c: Container => thing ++ c.sub.exprTraverse(f)
        case o: Operator => thing ++ o.left.exprTraverse(f) ++ o.right.exprTraverse(f)
        case Unity => thing
      }
    }
    def findAny(f: PartialFunction[SymOp, Boolean]): Boolean = {
      exprTraverse(f.andThen(Seq(_))).foldLeft(false)(_ || _)
    }
  }

  sealed trait Symbol extends SymOp
  case class Reference(value: String) extends Symbol
  case object Unity extends Symbol

  case class Func(name: String, arguments: Seq[Expr]) extends Expr
  case class Container(sub: SymOp) extends SymOp
  case class Operator(`type`: OperatorToken, left: SymOp, right: SymOp) extends SymOp {

  }

  def format(expr: Expr): String = expr match {
    case Func(n, args) => s"$n(${args.map(format).mkString(", ")}"
    case Container(sub) => s"(${format(sub)})"
    case Operator(Cross, l, r) => s"${format(l)} * ${format(r)}"
    case Operator(Nest, l, r) => s"${format(l)} / ${format(r)}"
    case Operator(Blend, l, r) => s"${format(l)} + ${format(r)}"
    case Reference(str) => str
    case Unity => "1"
  }
}