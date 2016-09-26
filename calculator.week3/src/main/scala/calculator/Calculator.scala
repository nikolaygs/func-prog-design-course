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
    namedExpressions map { 
      case (varName, expr) => (varName, Signal(eval(expr(), namedExpressions)))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {

    def evalInternal(expr: Expr)(implicit usedRefsInVar: Set[String]): Double = 
      expr match {
        case Literal(x)   => x
        case Plus(a, b)   => evalInternal(a) + evalInternal(b)
        case Minus(a, b)  => evalInternal(a) - evalInternal(b)
        case Times(a, b)  => evalInternal(a) * evalInternal(b)
        case Divide(a, b) => evalInternal(a) / evalInternal(b)
        case Ref(name)    => 
          if (usedRefsInVar contains name)
            Double.NaN
          else 
            evalInternal(getReferenceExpr(name, references))(usedRefsInVar + name)
      }

    evalInternal(expr)(Set.empty)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
