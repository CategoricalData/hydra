// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * A section expression
 */
public class SectionExpression implements Serializable, Comparable<SectionExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.SectionExpression");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  /**
   * The operator
   */
  public final hydra.haskell.syntax.Operator operator;

  /**
   * The operand
   */
  public final hydra.haskell.syntax.Expression expression;

  public SectionExpression (hydra.haskell.syntax.Operator operator, hydra.haskell.syntax.Expression expression) {
    this.operator = operator;
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SectionExpression)) {
      return false;
    }
    SectionExpression o = (SectionExpression) other;
    return java.util.Objects.equals(
      this.operator,
      o.operator) && java.util.Objects.equals(
      this.expression,
      o.expression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(operator) + 3 * java.util.Objects.hashCode(expression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SectionExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      operator,
      other.operator);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      expression,
      other.expression);
  }

  public SectionExpression withOperator(hydra.haskell.syntax.Operator operator) {
    return new SectionExpression(operator, expression);
  }

  public SectionExpression withExpression(hydra.haskell.syntax.Expression expression) {
    return new SectionExpression(operator, expression);
  }
}
