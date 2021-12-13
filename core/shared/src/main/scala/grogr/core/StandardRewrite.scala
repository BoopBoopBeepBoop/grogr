package grogr.core

import grogr.core.Expr.{Container, Operator, SymOp, Unity}
import grogr.core.Token.{Blend, Cross, Nest}

object StandardRewrite {
  val prelimRules: PartialFunction[SymOp, SymOp] = {
    // container collapse
    case Container(Container(inner)) =>
      var out = inner
      while (out.isInstanceOf[Container]) {
        out = out.asInstanceOf[Container].sub
      }
      Container(out)

    // nest expansion with blend
    case Operator(Nest, Container(Operator(Blend, left, right)), term) =>
      Operator(Blend, Operator(Nest, left, term), Operator(Nest, right, term))
    case Operator(Nest, term, Container(Operator(Blend, left, right))) =>
      Operator(Blend, Operator(Nest, term, left), Operator(Nest, term, right))

    // cross expansion with blend
    case Operator(Cross, Container(Operator(Blend, left, right)), term) =>
      Operator(Blend, Operator(Cross, left, term), Operator(Cross, right, term))
    case Operator(Cross, term, Container(Operator(Blend, left, right))) =>
      Operator(Blend, Operator(Cross, term, left), Operator(Cross, term, right))

    case Operator(Nest, left, Operator(other@(Cross|Blend) , left1, right)) =>
      Operator(other, Operator(Nest, left, left1), right)
    case Operator(Nest, Operator(other@(Cross|Blend), left, right1), right) =>
      Operator(Nest, left, Operator(other, right1, right))

    case Operator(Cross, left, Operator(Blend, left1, right)) =>
      Operator(Blend, Operator(Cross, left, left1), right)
    case Operator(Cross, Operator(Blend, left, right1), right) =>
      Operator(Blend, left, Operator(Cross, right1, right))
  }

  def unityRules(expr: SymOp) = {
    val terms = expr.terms
    val largestOrder = terms.map(_.factors.size).max

    val termModificationFunctions =
      terms
        .filter(_.factors.size < largestOrder)
        .map { in =>
          // TODO: mutability, consider a different way to achieve this in the future
          var alreadyReplaced = false
          val out: PartialFunction[SymOp, SymOp] = {
            case op if op == in && !alreadyReplaced =>
              alreadyReplaced = true
              val unityCrossesRequired = largestOrder - op.factors.size
              (1 to unityCrossesRequired).foldLeft(op) { case (acc, _) =>
                Operator(Cross, acc, Unity)
              }
          }

          out
        }

    val finalFunction = termModificationFunctions.foldLeft(PartialFunction.empty[SymOp, SymOp]) { (func, next) =>
      func.orElse(next)
    }

    finalFunction
  }
}
