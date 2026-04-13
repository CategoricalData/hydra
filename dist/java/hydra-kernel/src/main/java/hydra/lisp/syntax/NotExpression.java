// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * Logical negation: (not expr)
 */
public class NotExpression implements Serializable, Comparable<NotExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.NotExpression");

  public static final hydra.core.Name EXPRESSION = new hydra.core.Name("expression");

  /**
   * The operand expression
   */
  public final hydra.lisp.syntax.Expression expression;

  public NotExpression (hydra.lisp.syntax.Expression expression) {
    this.expression = expression;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NotExpression)) {
      return false;
    }
    NotExpression o = (NotExpression) other;
    return java.util.Objects.equals(
      this.expression,
      o.expression);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(expression);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NotExpression other) {
    return hydra.util.Comparing.compare(
      expression,
      other.expression);
  }
}
