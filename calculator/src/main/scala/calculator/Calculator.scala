package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {

    namedExpressions.map {
      case (variableName, expression) =>
          (variableName, Signal(eval(expression(), namedExpressions)))
    }
  }

  def eval(expression: Expr, references: Map[String, Signal[Expr]]): Double = expression match {
    case Literal(v) => v
    case r: Ref =>

      if (hasCycles(r, references, Set())) {
        Double.NaN
      } else {
        eval(getReferenceExpr(r.name, references), references)
      }

    case Plus(a, b) => eval(a, references) + eval(b, references)
    case Minus(a, b) => eval(a, references) - eval(b, references)
    case Times(a, b) => eval(a, references) * eval(b, references)
    case Divide(a, b) => eval(a, references) / eval(b, references)
    case _ => Double.NaN
  }

  private def hasCycles(expr: Expr, references: Map[String, Signal[Expr]], visited: Set[String]): Boolean = {
    expr match {
      case Ref(n) if visited.contains(n) => true
      case ref: Ref => {

        val refExpr = getReferenceExpr(ref.name, references)

        refExpr match {
          case Ref(n) if visited.contains(n) => true
          case Ref(n) => hasCycles(refExpr ,references, visited + ref.name)
          case _ => hasCycles(refExpr ,references, visited + ref.name)
        }
      }
      case Plus(a, b) => hasCycles(a, references, visited) || hasCycles(b, references, visited)
      case Minus(a, b) => hasCycles(a, references, visited) || hasCycles(b, references, visited)
      case Times(a, b) => hasCycles(a, references, visited) || hasCycles(b, references, visited)
      case Divide(a, b) => hasCycles(a, references, visited) || hasCycles(b, references, visited)
      case Literal(v) => false
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]): Expr = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
